const API_URL = "http://localhost:3000";

// STATE
let gameState = null;
let activeSeat = -1;  
let selectedCards = []; // [{ playerId, cardIdx }]
let lastInfoHash = ""; 

// UTILS
const getPhaseTag = (p) => (typeof p === 'object') ? p.tag : p;
const getPhaseData = (p) => (typeof p === 'object') ? p : {};

// --- POLLING LOOP ---
async function fetchState() {
    try {
        const res = await fetch(`${API_URL}/game/state`);
        const newState = await res.json();
        if (!newState) return;

        const phaseTag = getPhaseTag(newState.phase);
        
        // 1. GAME OVER
        if (phaseTag === "GameOver") {
            gameState = newState;
            document.getElementById('turn-overlay').classList.add('hidden');
            document.getElementById('game-over-overlay').classList.remove('hidden');
            
            let tableHtml = `
                <table style="width:100%; color:white; margin-top:10px; border-collapse: collapse; font-size:0.9rem;">
                    <tr style="border-bottom: 2px solid var(--gold);">
                        <th>Pemain</th>
                        <th>Kartu</th>
                        <th>Score Tangan</th>
                        <th>Poin Liga</th>
                    </tr>
            `;

            // Kita sort agar Juara (Score Tangan terendah) di atas
            const sortedPlayers = [...gameState.players].sort((a, b) => {
                const scoreA = (a.hand.contents || []).reduce((acc, c) => acc + getCardValue(c), 0);
                const scoreB = (b.hand.contents || []).reduce((acc, c) => acc + getCardValue(c), 0);
                return scoreA - scoreB;
            });

            sortedPlayers.forEach(p => {
                const hand = p.hand.contents || [];
                // Hitung total value menggunakan Logic JS yang sudah disamakan dengan Haskell
                const totalScore = hand.reduce((acc, c) => acc + getCardValue(c), 0);
                
                const cardDisplay = hand.map(c => {
                    const val = getCardValue(c);
                    const color = (c.suit === 'Hearts' || c.suit === 'Diamonds' || c.suit === 'Red') ? '#e74c3c' : '#bdc3c7';
                    return `<span style="color:${color}; font-weight:bold;">${getRankShort(c.rank)}</span><sub style="font-size:0.6em">(${val})</sub>`;
                }).join(" ");

                // Highlight baris player kita
                const bg = (p.playerId === activeSeat) ? 'background:rgba(255,255,255,0.1);' : '';

                tableHtml += `
                    <tr style="${bg} border-bottom:1px solid rgba(255,255,255,0.1);">
                        <td style="padding:8px;">P${p.playerId}</td>
                        <td style="padding:8px;">${cardDisplay}</td>
                        <td style="padding:8px; font-weight:bold; color:#f1c40f; font-size:1.2em;">${totalScore}</td>
                        <td style="padding:8px;">${p.matchPoints}</td>
                    </tr>
                `;
            });
            tableHtml += `</table>`;

            const winnerLog = newState.logs.find(l => l.includes("Winner")) || "Game Selesai";
            document.getElementById('winner-text').innerHTML = `<h2 style="color:#2ecc71">${winnerLog}</h2>` + tableHtml;
            
            return;
        }
        
        function getCardValue(c) {
            const r = c.rank;
            const s = c.suit;

            // 1. JOKER RULES
            if (r === 'Joker') {
                if (s === 'Red') return -1;   // Joker Merah = -1
                if (s === 'Black') return 15; // Joker Hitam = 15
                return 0; // Fallback
            }

            // 2. KING RULES
            if (r === 'King') {
                if (s === 'Hearts' || s === 'Diamonds') return 0; // King Merah = 0
                if (s === 'Clubs' || s === 'Spades') return 13;   // King Hitam (Standard)
            }

            // 3. SPECIAL RULES (Jack/Queen)
            if (r === 'Jack' || r === 'Queen') return 11; // Di Card.hs valueRules Jack/Queen = 11

            // 4. NUMBER CARDS
            const map = {
                'Ace': 1, 'Two': 2, 'Three': 3, 'Four': 4, 'Five': 5,
                'Six': 6, 'Seven': 7, 'Eight': 8, 'Nine': 9, 'Ten': 10
            };
            
            return map[r] || 0;
        }

        // 2. CHECK PRIVATE INFO (Peek result)
        if (activeSeat !== -1 && newState.privateInfo) {
            const myInfos = newState.privateInfo.filter(i => i[0] === activeSeat).map(i => i[1]);
            if (myInfos.length > 0) {
                const hash = myInfos.join("|");
                if (hash !== lastInfoHash) {
                    lastInfoHash = hash;
                    showInfoModal(myInfos);
                }
            }
        }

        // 3. DETERMINE TURN
        let effectiveTurn = newState.currentTurn;
        if (phaseTag === 'TimpaRound') {
            effectiveTurn = newState.phase.askingIdx; // Giliran orang yang ditanya Timpa
        }

        // 4. HOTSEAT SWITCH
        if (effectiveTurn !== activeSeat && phaseTag !== 'InitPeekFeedback') {
            showTurnOverlay(effectiveTurn);
            gameState = newState; 
            // Pause render until clicked "Ready"
        } else {
            gameState = newState;
            renderTable();
        }

    } catch (e) { console.error(e); }
}
setInterval(fetchState, 1000);
fetchState();

// --- RENDER TABLE ---
function renderTable() {
    if (activeSeat === -1 || !gameState) return;

    const phaseTag = getPhaseTag(gameState.phase);
    
    // 1. UPDATE HEADER
    let phaseText = phaseTag;
    if(phaseTag === 'TimpaRound') phaseText = `TIMPA! (Rank: ${getRankShort(gameState.phase.targetRank)})`;
    if(phaseTag === 'ResolvePowerup') phaseText = `POWERUP: ${gameState.phase.contents || "Active"}`;
    document.getElementById('phase-display').innerText = phaseText;
    
    // Logs
    const logEl = document.getElementById('game-logs');
    logEl.innerHTML = "";
    gameState.logs.slice().reverse().slice(0, 3).forEach(l => {
        logEl.innerHTML += `<li>> ${l}</li>`;
    });

    // 2. RENDER PLAYERS (ROTATED)
    // Urutan: 0 -> 1 -> 2 -> 3
    // Kita ingin activeSeat selalu di Bottom.
    // Misal activeSeat = 1. Bottom=1, Right=2, Top=3, Left=0.
    const seats = [
        { id: 'seat-bottom', offset: 0 },
        { id: 'seat-right', offset: 1 },
        { id: 'seat-top', offset: 2 },
        { id: 'seat-left', offset: 3 }
    ];

    seats.forEach(pos => {
        const targetPid = (activeSeat + pos.offset) % 4;
        const player = gameState.players.find(p => p.playerId === targetPid);
        const el = document.getElementById(pos.id);
        
        // Render Hand
        // Bottom (Kita) bisa diklik, yang lain tergantung powerup
        const isMine = (pos.offset === 0);
        renderPlayerSeat(el, player, isMine);
    });

    // 3. CENTER AREA
    const deckCount = gameState.drawDeck ? gameState.drawDeck.length : 0;
    document.getElementById('deck-count').innerText = deckCount;
    
    const discardEl = document.getElementById('discard-pile');
    if (gameState.discardPile.length > 0) {
        const top = gameState.discardPile[0];
        discardEl.className = `card ${isRed(top.suit) ? 'red' : 'black'}`;
        discardEl.innerHTML = renderCardInner(top);
    } else {
        discardEl.className = "card placeholder";
        discardEl.innerHTML = "Empty";
    }

    // 4. UPDATE BUTTONS
    updateControls(phaseTag);
}

function renderPlayerSeat(container, player, isMine) {
    if (isMine) {
        // Update Info Player Sendiri
        const nameEl = document.getElementById('my-name');
        const scoreEl = document.getElementById('my-score');
        const handDiv = document.getElementById('my-hand');

        if (nameEl) nameEl.innerText = `P${player.playerId} (ANDA)`;
        if (scoreEl) scoreEl.innerText = `Pts: ${player.matchPoints}`;
        
        // Render Tangan Kita
        if (handDiv) renderHand(handDiv, player, true);
    } else {
        // Render Info Lawan (HTML Injection)
        container.innerHTML = `
            <div style="margin-bottom:5px; font-weight:bold; text-shadow: 1px 1px 2px black;">
                P${player.playerId} (${player.matchPoints})
            </div>
            <div class="hand-container" id="hand-${player.playerId}"></div>
        `;
        const handDiv = container.querySelector('.hand-container');
        if (handDiv) renderHand(handDiv, player, false);
    }
}

function renderHand(container, player, isMine) {
    container.innerHTML = "";
    
    // [FIX UTAMA DI SINI]
    // Cek apakah player.hand itu Array langsung atau Object {contents: ...}
    let cards = [];
    if (Array.isArray(player.hand)) {
        cards = player.hand;
    } else if (player.hand && Array.isArray(player.hand.contents)) {
        cards = player.hand.contents;
    } else {
        console.warn("Format Hand salah:", player.hand);
        cards = [];
    }
    
    const phaseTag = getPhaseTag(gameState.phase);
    // Kartu baru hasil draw ada di index 0 saat DiscardPhase
    const newCardIdx = (isMine && phaseTag === 'DiscardPhase') ? 0 : -1;

    cards.forEach((card, idx) => {
        const div = document.createElement('div');
        div.className = "card";
        
        // Logic Terbuka/Tertutup
        // Default tertutup. Terbuka jika:
        // 1. Kartu itu baru diambil (DiscardPhase index 0)
        // 2. Ini fase InitPeekFeedback (sementara kita buka semua biar gampang hafal)
        const isOpen = (idx === newCardIdx) || (isMine && phaseTag === 'InitPeekFeedback');

        if (isOpen) {
            div.classList.add(isRed(card.suit) ? 'red' : 'black');
            div.innerHTML = renderCardInner(card);
        } else {
            div.classList.add("back");
        }

        // Seleksi Visual
        const isSelected = selectedCards.some(s => s.playerId === player.playerId && s.cardIdx === idx);
        if (isSelected) div.classList.add("selected");

        // Interaction Handler
        div.onclick = () => handleCardClick(player.playerId, idx);
        
        container.appendChild(div);
    });
}

// --- LOGIKA KLIK KARTU (KOMPLEKS) ---
function handleCardClick(pid, idx) {
    const phase = getPhaseTag(gameState.phase);
    const isMine = (pid === activeSeat);

    // 1. INIT PEEK (Hanya kartu sendiri, max 2)
    if (phase === "InitialPeekPhase") {
        if (!isMine) return;
        toggleSelection(pid, idx, 2);
    }
    // 2. DISCARD (Hanya kartu sendiri, max 1)
    else if (phase === "DiscardPhase") {
        if (!isMine) return;
        if (confirm("Buang kartu ini?")) sendAction("discard", idx);
    }
    // 3. TIMPA (Hanya kartu sendiri, max 1)
    else if (phase === "TimpaRound") {
        if (!isMine) return;
        selectedCards = [{ playerId: pid, cardIdx: idx }]; // Auto replace
        renderTable();
    }
    // 4. POWERUP (Sesuai Tipe)
    else if (phase === "ResolvePowerup") {
        const pType = gameState.phase.contents; // e.g., "PeekSelf", "Switch"
        handlePowerupSelection(pType, pid, idx);
    }
}

function handlePowerupSelection(type, pid, idx) {
    const isMine = (pid === activeSeat);
    
    switch (type) {
        case "PeekSelf":
            if (isMine) toggleSelection(pid, idx, 1);
            break;
        case "PeekOpponent":
            if (!isMine) toggleSelection(pid, idx, 1);
            break;
        case "PeekSO":
        case "Switch":
        case "PeekSwitch":
            // Total 2 kartu (1 kita, 1 lawan). 
            // Kita pakai limit buffer 2, validasi strict ada di isValidPowerupSelection
            toggleSelection(pid, idx, 2); 
            break;
        case "PeekDouble": 
            // Total 4 kartu (2 kita, 2 lawan).
            toggleSelection(pid, idx, 4);
            break;
        default:
            console.warn("Unknown Powerup", type);
    }
}

function toggleSelection(pid, idx, max) {
    const foundIdx = selectedCards.findIndex(s => s.playerId === pid && s.cardIdx === idx);
    if (foundIdx >= 0) {
        selectedCards.splice(foundIdx, 1);
    } else {
        if (selectedCards.length >= max) selectedCards.shift(); // Remove oldest
        selectedCards.push({ playerId: pid, cardIdx: idx });
    }
    renderTable();
}

// --- CONTROLS & ACTIONS ---
function updateControls(phase) {
    // Hide All First
    document.querySelectorAll('.btn-group .btn').forEach(b => b.classList.add('hidden'));
    const instruct = document.getElementById('instruction');

    if (phase === "InitialPeekPhase") {
        instruct.innerText = "Pilih 2 kartu Anda untuk diintip.";
        if (selectedCards.length === 2) {
            const btn = document.getElementById('btn-use-powerup');
            btn.classList.remove('hidden'); btn.innerText = "INTIP (2)";
            btn.onclick = () => sendAction("initPeek", selectedCards.map(s => s.cardIdx));
        }
    }
    else if (phase === "InitPeekFeedback") {
        instruct.innerText = "Hafalkan kartu Anda.";
        const btn = document.getElementById('btn-finish');
        btn.classList.remove('hidden'); btn.innerText = "LANJUT";
    }
    else if (phase === "DrawPhase") {
        instruct.innerText = "Giliran Anda: Ambil Kartu dari Deck.";
        document.getElementById('btn-draw').classList.remove('hidden');
    }
    else if (phase === "DiscardPhase") {
        instruct.innerText = "Pilih 1 kartu untuk dibuang. (Kiri = Kartu Baru)";
    }
    else if (phase === "TimpaRound") {
        instruct.innerText = "Ada kartu sama? Timpa atau Pass.";
        document.getElementById('btn-pass-stack').classList.remove('hidden');
        if (selectedCards.length === 1) document.getElementById('btn-stack').classList.remove('hidden');
    }
    else if (phase === "PostRoundDecision") {
        instruct.innerText = "Akhir Giliran.";
        document.getElementById('btn-kabul').classList.remove('hidden');
        document.getElementById('btn-finish').classList.remove('hidden');
    }
    else if (phase === "ResolvePowerup") {
        const type = gameState.phase.contents;
        instruct.innerText = `POWERUP: ${type}. Pilih Target lalu Gunakan/Skip.`;
        document.getElementById('btn-skip-powerup').classList.remove('hidden');
        
        // Cek validitas seleksi sebelum tombol muncul
        if (isValidPowerupSelection(type)) {
            const btn = document.getElementById('btn-use-powerup');
            btn.classList.remove('hidden'); 
            btn.innerText = (type.includes("Switch")) ? "TUKAR" : "GUNAKAN";
            btn.onclick = actionUsePowerup;
        }
    }
}

function isValidPowerupSelection(type) {
    const mine = selectedCards.filter(s => s.playerId === activeSeat).length;
    const opp = selectedCards.filter(s => s.playerId !== activeSeat).length;

    if (type === "PeekSelf") return mine === 1 && opp === 0;
    if (type === "PeekOpponent") return mine === 0 && opp === 1;
    
    // PeekSO, Switch, PeekSwitch butuh 1 Saya + 1 Lawan
    if (type === "PeekSO" || type === "Switch" || type === "PeekSwitch") {
        return mine === 1 && opp === 1;
    }
    
    // PeekDouble butuh 2 Saya + 2 Lawan (Total 4)
    if (type === "PeekDouble") {
        return mine === 2 && opp === 2;
    }
    
    return false;
}

// --- SEND ACTION ---
async function sendAction(type, payload) {
    let body = {};
    const pid = activeSeat;

    if (type === 'initPeek') body = { tag: 'InitPeekAction', contents: [pid, payload[0], payload[1]] };
    else if (type === 'draw') body = { tag: 'DrawAction', contents: pid };
    else if (type === 'discard') body = { tag: 'DiscardAction', contents: [pid, payload] };
    else if (type === 'stack') body = { tag: 'TimpaAction', contents: [pid, selectedCards[0].cardIdx] };
    else if (type === 'passStack') body = { tag: 'PassTimpaAction', contents: pid };
    else if (type === 'skipPowerup') body = { tag: 'SkipPowerupAction', contents: pid };
    else if (type === 'kabul') body = { tag: 'KabulAction', contents: pid };
    else if (type === 'finish') body = { tag: 'FinishTurnAction', contents: pid };
    
    // ACTION POWERUP TARGET
    else if (type === 'target') {
        // Backend mengharapkan: TargetAction pid targetPid [indices]
        // Tapi powerup kompleks melibatkan kartu diri sendiri dan musuh.
        // Kita kirim daftar indeks semua kartu yang terlibat.
        // Backend (GameEngine) harus pintar memisahkan mana milik user mana milik lawan
        // ATAU kita kirim format generik: [mineIdx, oppIdx] dan targetID = OpponentID.
        
        const oppSelection = selectedCards.find(s => s.playerId !== activeSeat);
        const targetPid = oppSelection ? oppSelection.playerId : activeSeat;
        
        // Kumpulkan semua indeks kartu (milik sendiri maupun lawan)
        // Kita urutkan agar konsisten: [PunyaKita..., PunyaLawan...]
        const myIndices = selectedCards.filter(s => s.playerId === activeSeat).map(s => s.cardIdx);
        const oppIndices = selectedCards.filter(s => s.playerId !== activeSeat).map(s => s.cardIdx);
        const allIndices = [...myIndices, ...oppIndices];

        body = { tag: 'TargetAction', contents: [pid, targetPid, allIndices] };
    }

    await fetch(`${API_URL}/game/action`, {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body)
    });
    
    selectedCards = [];
    fetchState();
}

// WRAPPER BUTTONS
function actionDraw() { sendAction('draw'); }
function actionStack() { sendAction('stack'); }
function actionPassStack() { sendAction('passStack'); }
function actionSkipPowerup() { sendAction('skipPowerup'); }
function actionUsePowerup() { sendAction('target'); }
function actionKabul() { sendAction('kabul'); }
function actionFinishTurn() { sendAction('finish'); }
function resetGame() { fetch(`${API_URL}/game/reset`, { method: 'POST' }).then(() => { activeSeat=-1; fetchState(); }); }

// HELPERS VISUAL
function showTurnOverlay(pid) {
    document.getElementById('next-player-name').innerText = `Player ${pid}`;
    document.getElementById('turn-overlay').classList.remove('hidden');
}
function confirmTurnStart() {
    if(!gameState) return;
    const pTag = getPhaseTag(gameState.phase);
    activeSeat = (pTag === 'TimpaRound') ? gameState.phase.askingIdx : gameState.currentTurn;
    document.getElementById('turn-overlay').classList.add('hidden');
    selectedCards = []; lastInfoHash = "";
    closeInfoModal();
    renderTable();
}

function renderCardInner(c) {
    const r = getRankShort(c.rank);
    const s = getSuitSymbol(c.suit);
    let p = "";
    if (c.powerup !== 'Normal') p = `<div style="font-size:0.5em; position:absolute; bottom:5px;">${c.powerup.replace('Peek','👁').replace('Switch','⇄')}</div>`;
    return `<div>${r}</div><div style="font-size:1.5em">${s}</div><div>${r}</div>${p}`;
}
function isRed(s) { return ['Hearts','Diamonds','Red'].includes(s); }
function getRankShort(r) { const m = {'Ace':'A','King':'K','Queen':'Q','Jack':'J','Ten':'10'}; return m[r] || r; }
function getSuitSymbol(s) { const m = {'Hearts':'♥','Diamonds':'♦','Spades':'♠','Clubs':'♣','Red':'★','Black':'★'}; return m[s] || s; }

function showInfoModal(msgs) {
    document.getElementById('info-overlay').classList.remove('hidden');
    document.getElementById('info-content').innerHTML = msgs.map(m => `<p>${m}</p>`).join("");
}
function closeInfoModal() { document.getElementById('info-overlay').classList.add('hidden'); }