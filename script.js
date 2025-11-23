const API_URL = "http://localhost:3000";

// --- GLOBAL STATE ---
let gameState = null;
let activeSeat = -1;  
let selectedIndices = []; 
let lastInfoHash = ""; 

// --- UTILITIES ---
const getPhaseTag = (phase) => (typeof phase === 'object') ? phase.tag : phase;

// --- LOOP UTAMA ---
async function fetchState() {
    try {
        const res = await fetch(`${API_URL}/game/state`);
        const newState = await res.json();
        if (!newState) return;

        // 1. Cek Game Over
        const phaseTag = getPhaseTag(newState.phase);
        if (phaseTag === "GameOver") {
            gameState = newState;
            document.getElementById('turn-overlay').classList.add('hidden');
            document.getElementById('game-over-overlay').classList.remove('hidden');
            document.getElementById('winner-text').innerText = newState.logs[newState.logs.length - 1] || "Game Selesai";
            return;
        }

        // 2. CEK INFO RAHASIA (Fix Utama)
        // Cek apakah ada pesan rahasia untuk 'activeSeat' (kita) di state terbaru
        // Lakukan ini SEBELUM memblokir layar dengan overlay giliran
        if (activeSeat !== -1) {
            checkPrivateInfo(activeSeat, newState);
        }

        // 3. Cek Giliran (Hotseat Logic)
        if (newState.currentTurn !== activeSeat) {
            showTurnOverlay(newState.currentTurn);
            gameState = newState; 
        } else {
            gameState = newState;
            renderTable();
        }

    } catch (e) {
        console.error("Polling Error:", e);
    }
}

// Start Polling
setInterval(fetchState, 1000);
fetchState();

// --- RENDER FUNCTIONS ---
function renderTable() {
    // Info Umum
    const phaseTag = getPhaseTag(gameState.phase);
    document.getElementById('phase-display').innerText = `FASE: ${formatPhaseName(phaseTag)}`;
    
    // Logs
    const logList = document.getElementById('game-logs');
    logList.innerHTML = "";
    gameState.logs.slice().reverse().slice(0, 5).forEach(log => {
        const li = document.createElement('li');
        li.innerText = "> " + log;
        logList.appendChild(li);
    });

    // Setup Pemain
    const myId = activeSeat;
    const oppId = (activeSeat + 1) % gameState.players.length;
    const myPlayer = gameState.players.find(p => p.playerId === myId);
    const oppPlayer = gameState.players.find(p => p.playerId === oppId);

    // Render Bottom (Kita)
    document.getElementById('bottom-player-name').innerText = `ANDA (P${myId})`;
    document.getElementById('bottom-score').innerText = `Pts: ${myPlayer.score || 0}`;
    renderHand(document.getElementById('hand-bottom'), myPlayer.hand, true, true);

    // Render Top (Lawan)
    document.getElementById('top-player-name').innerText = `Lawan (P${oppId})`;
    document.getElementById('top-score').innerText = `Pts: ${oppPlayer.score || 0}`;
    renderHand(document.getElementById('hand-top'), oppPlayer.hand, false, false);

    // Render Pemain Lain
    renderOtherPlayers(gameState.players, myId, oppId);

    // Deck & Discard
    const deckEl = document.getElementById('deck');
    const deckCount = gameState.drawDeck ? gameState.drawDeck.length : 0;
    document.getElementById('deck-count').innerText = deckCount;
    
    if (phaseTag === "DrawPhase" && !document.getElementById('btn-draw').disabled) {
        deckEl.classList.add('glow-effect');
        deckEl.style.cursor = "pointer";
    } else {
        deckEl.classList.remove('glow-effect');
        deckEl.style.cursor = "default";
    }

    const discardDiv = document.getElementById('discard-pile');
    if (gameState.discardPile.length > 0) {
        const topCard = gameState.discardPile[0];
        discardDiv.className = "card";
        discardDiv.innerHTML = createCardHTML(topCard);
        const colorClass = (topCard.suit === 'Hearts' || topCard.suit === 'Diamonds' || topCard.suit === 'Red') ? 'red' : 'black';
        discardDiv.classList.add(colorClass);
    } else {
        discardDiv.className = "card-placeholder";
        discardDiv.innerHTML = "<span>Discard<br>Pile</span>";
    }

    updateControls(phaseTag);
    // Kita tidak perlu panggil checkPrivateInfo di sini lagi karena sudah di fetchState
}

function renderHand(container, handData, isMine, isOpen) {
    container.innerHTML = "";
    const cards = handData.contents ? handData.contents : handData;

    cards.forEach((card, index) => {
        const div = document.createElement('div');
        div.className = "card";

        if (isOpen) {
            const colorClass = (card.suit === 'Hearts' || card.suit === 'Diamonds' || card.suit === 'Red') ? 'red' : 'black';
            div.classList.add(colorClass);
            div.innerHTML = createCardHTML(card);
            if (isMine && selectedIndices.includes(index)) div.classList.add("selected");
            if (isMine) div.onclick = () => handleCardInteraction(index);
        } else {
            div.classList.add("back");
        }
        container.appendChild(div);
    });
}

function createCardHTML(card) {
    const suitSymbol = getSuitSymbol(card.suit);
    const rankShort = getRankShort(card.rank);
    return `
        <div class="corner top-left"><span>${rankShort}</span><span>${suitSymbol}</span></div>
        <div class="suit-center">${suitSymbol}</div>
        <div class="corner bottom-right"><span>${rankShort}</span><span>${suitSymbol}</span></div>
        ${card.powerup !== 'Normal' ? `<div style="position:absolute; bottom:20%; font-size:0.6rem; opacity:0.7;">${getPowerupShort(card.powerup)}</div>` : ''}
    `;
}

function getSuitSymbol(suit) {
    const map = { 'Hearts': '♥', 'Diamonds': '♦', 'Clubs': '♣', 'Spades': '♠', 'Red': '★', 'Black': '★' };
    return map[suit] || '?';
}

function getRankShort(rank) {
    const map = { 'Ace': 'A', 'Two': '2', 'Three': '3', 'Four': '4', 'Five': '5', 'Six': '6', 'Seven': '7', 'Eight': '8', 'Nine': '9', 'Ten': '10', 'Jack': 'J', 'Queen': 'Q', 'King': 'K', 'Joker': 'JK' };
    return map[rank] || rank;
}

function getPowerupShort(powerup) {
    return powerup.replace('Peek', '👁').replace('Switch', '⇄').replace('Opponent', 'Opp').replace('Self', 'Me');
}

// --- INTERACTION ---
function handleCardInteraction(index) {
    const phase = getPhaseTag(gameState.phase);
    if (phase === "DiscardPhase") {
        if(confirm("Buang kartu ini?")) sendAction('discard', index);
    } else if (phase === "ResolvePowerup") {
        const pos = selectedIndices.indexOf(index);
        if (pos > -1) selectedIndices.splice(pos, 1);
        else selectedIndices.push(index);
        renderTable();
    }
}

function updateControls(phase) {
    const btnDraw = document.getElementById('btn-draw');
    const btnConfirm = document.getElementById('btn-confirm');
    const instruct = document.getElementById('instruction-text');

    btnDraw.disabled = true;
    btnConfirm.classList.add('hidden');
    const activeHand = document.querySelector('.active-hand');
    if (activeHand) activeHand.style.border = "none";

    if (phase === "DrawPhase") {
        instruct.innerHTML = "Giliran Anda. <span style='color:#f1c40f'>Ambil Kartu</span>.";
        btnDraw.disabled = false;
    } else if (phase === "DiscardPhase") {
        instruct.innerHTML = "Hand Penuh. <span style='color:#e74c3c'>Pilih 1 kartu buang</span>.";
        if (activeHand) activeHand.style.border = "2px dashed rgba(231, 76, 60, 0.5)";
    } else if (phase === "ResolvePowerup") {
        instruct.innerHTML = `Powerup Aktif! Pilih target.`;
        btnConfirm.classList.remove('hidden');
        btnConfirm.innerText = `KONFIRMASI (${selectedIndices.length})`;
    }
}

// --- INFO MODAL (LOGIC BARU) ---
function checkPrivateInfo(myId, stateSource) {
    // Gunakan stateSource (newState) yang dilempar dari fetchState
    const source = stateSource || gameState; 
    if (!source || !source.privateInfo) return;

    // Filter info hanya untuk saya
    const myInfos = source.privateInfo.filter(info => info[0] === myId).map(i => i[1]);
    
    if (myInfos.length > 0) {
        const contentHash = myInfos.join("|");
        // Pastikan info ini belum ditampilkan sebelumnya
        if (contentHash !== lastInfoHash) {
            lastInfoHash = contentHash;
            showInfoModal(myInfos);
        }
    } else {
        // Jangan reset lastInfoHash sembarangan agar tidak muncul ulang saat polling
    }
}

function showInfoModal(messages) {
    const modal = document.getElementById('info-overlay');
    const content = document.getElementById('info-content');
    // Format pesan
    content.innerHTML = messages.map(msg => `<p>${prettifyInfo(msg)}</p>`).join("");
    modal.classList.remove('hidden');
}

function closeInfoModal() {
    document.getElementById('info-overlay').classList.add('hidden');
}

function prettifyInfo(msg) {
    return msg.replace(/([a-zA-Z]+) of ([a-zA-Z]+)/g, (match, r, s) => {
        const sym = getSuitSymbol(s);
        const col = (s==='Hearts'||s==='Diamonds'||s==='Red') ? 'red' : 'black';
        return `<span style="color:${col === 'red' ? '#e74c3c' : '#bdc3c7'}; font-weight:bold; background:#fff; padding:2px 6px; border-radius:4px; box-shadow:1px 1px 2px #000;">${r} ${sym}</span>`;
    });
}

// --- ACTIONS ---
async function sendAction(type, payload) {
    let body = {};
    const pid = activeSeat;
    if (type === 'draw') body = { tag: 'DrawAction', contents: pid };
    else if (type === 'discard') body = { tag: 'DiscardAction', contents: [pid, payload] };
    else if (type === 'target') body = { tag: 'TargetAction', contents: [pid, payload] };
    else if (type === 'reset') return resetGame();

    const res = await fetch(`${API_URL}/game/action`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body)
    });
    const json = await res.json();
    if (json.error) alert(json.error);
    else {
        selectedIndices = [];
        fetchState();
    }
}

function actionDraw() { sendAction('draw'); }
function actionConfirmTarget() { sendAction('target', selectedIndices); }
function resetGame() { 
    fetch(`${API_URL}/game/reset`, { method: 'POST' }).then(() => {
        activeSeat = -1;
        document.getElementById('game-over-overlay').classList.add('hidden');
        lastInfoHash = "";
        fetchState();
    }); 
}

// --- UI HELPERS ---
function showTurnOverlay(nextPid) {
    const overlay = document.getElementById('turn-overlay');
    document.getElementById('next-player-name').innerText = `Player ${nextPid}`;
    overlay.classList.remove('hidden');
    // Note: Kita JANGAN closeInfoModal() disini, agar info tetap terlihat di atas overlay
}

function confirmTurnStart() {
    if (gameState) {
        activeSeat = gameState.currentTurn;
        document.getElementById('turn-overlay').classList.add('hidden');
        closeInfoModal(); // Tutup info lama saat player baru mulai
        lastInfoHash = ""; // Reset hash untuk player baru
        selectedIndices = [];
        renderTable();
    }
}

function renderOtherPlayers(players, myId, oppId) {
    const container = document.getElementById('other-players');
    container.innerHTML = "";
    players.forEach(p => {
        if (p.playerId === myId || p.playerId === oppId) return;
        const div = document.createElement('div');
        div.className = "mini-player";
        const handSize = (p.hand.contents || p.hand || []).length;
        let cardsHtml = "";
        for(let i=0; i<handSize; i++) cardsHtml += `<div class="mini-card-icon"></div>`;
        div.innerHTML = `<div><strong>P${p.playerId}</strong></div><div style="font-size:0.6em">Pts: ${p.score}</div><div class="mini-cards-row">${cardsHtml}</div>`;
        container.appendChild(div);
    });
}

function formatPhaseName(tag) {
    if (tag === 'DrawPhase') return "AMBIL KARTU";
    if (tag === 'DiscardPhase') return "BUANG KARTU";
    if (tag === 'ResolvePowerup') return "TARGET POWERUP";
    return tag.toUpperCase();
}