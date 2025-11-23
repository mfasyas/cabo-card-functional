const API_URL = "http://localhost:3000";

// --- GLOBAL STATE ---
let gameState = null;
let activeSeat = -1;  // Siapa yang sedang pegang HP? (-1 saat awal)
let selectedIndices = []; // Kartu yang dipilih (Array of Int)

// --- LOOP UTAMA ---
async function fetchState() {
    try {
        const res = await fetch(`${API_URL}/game/state`);
        const newState = await res.json();
        
        // Cek apakah game state valid
        if (!newState) return;

        // Cek Game Over
        const phaseTag = getPhaseTag(newState.phase);
        if (phaseTag === "GameOver") {
            gameState = newState;
            renderGameOver();
            return;
        }

        // --- LOGIKA HOTSEAT (Pass & Play) ---
        // Jika Server bilang giliran X, tapi yang duduk sekarang Y (atau belum ada)
        // Maka stop rendering, munculkan tirai hitam.
        if (newState.currentTurn !== activeSeat) {
            showTurnOverlay(newState.currentTurn);
            // Kita simpan state di background, tapi belum dirender ke meja
            gameState = newState; 
        } else {
            // Jika orangnya sudah benar, update tampilan meja real-time
            gameState = newState;
            renderTable(); 
        }

    } catch (e) {
        console.error("Koneksi error:", e);
    }
}

// Polling setiap 1 detik
setInterval(fetchState, 1000);
fetchState(); // Call pertama

// --- FUNGSI RENDER UTAMA ---
function renderTable() {
    // 1. Render Info Umum
    document.getElementById('phase-display').innerText = `Fase: ${getPhaseTag(gameState.phase)}`;
    document.getElementById('turn-display').innerText = `Giliran: Player ${gameState.currentTurn}`;
    
    // Render Logs
    const logList = document.getElementById('game-logs');
    logList.innerHTML = "";
    gameState.logs.slice().reverse().slice(0, 4).forEach(log => {
        const li = document.createElement('li');
        li.innerText = "> " + log;
        logList.appendChild(li);
    });

    // 2. TENTUKAN POSISI DUDUK
    // activeSeat (Kita) selalu di Index 0 (Bawah) secara visual
    // Lawan selalu di Index 1 (Atas) secara visual
    const myId = activeSeat;
    const oppId = (activeSeat + 1) % gameState.players.length; 

    const myPlayer = gameState.players.find(p => p.playerId === myId);
    const oppPlayer = gameState.players.find(p => p.playerId === oppId);

    // 3. Render Hand Bawah (PUNYA KITA - BISA KLIK)
    document.getElementById('bottom-player-name').innerText = `ANDA (Player ${myId})`;
    document.getElementById('bottom-score').innerText = `Score: ${myPlayer.score || 0}`;
    renderHand(document.getElementById('hand-bottom'), myPlayer.hand, true, true); // true = milik kita, true = terbuka

    // 4. Render Hand Atas (PUNYA LAWAN - TERTUTUP)
    document.getElementById('top-player-name').innerText = `Lawan (Player ${oppId})`;
    document.getElementById('top-score').innerText = `Score: ${oppPlayer.score || 0}`;
    renderHand(document.getElementById('hand-top'), oppPlayer.hand, false, false); // false = bukan kita, false = tertutup

    // 5. Render Discard Pile
    const discardDiv = document.getElementById('discard-pile');
    if (gameState.discardPile.length > 0) {
        const topCard = gameState.discardPile[0];
        discardDiv.className = "card";
        discardDiv.innerHTML = getCardHTML(topCard);
    } else {
        discardDiv.className = "card-placeholder";
        discardDiv.innerHTML = "Kosong";
    }

    // Update deck count badge (supports either `deck` or `drawPile` naming)
    const deckCountEl = document.getElementById('deck-count');
    if (deckCountEl) {
        const deckCount = gameState.deck ? gameState.deck.length : (gameState.drawPile ? gameState.drawPile.length : 0);
        deckCountEl.innerText = deckCount;
    }

    // 6. Update Tombol & Instruksi
    updateControls();
    
    // 7. Cek Info Rahasia (Peek)
    checkPrivateInfo(myId);
}

// --- RENDER COMPONENT ---
function renderHand(container, handData, isMine, isOpen) {
    container.innerHTML = "";
    // Handle struktur data Haskell: Hand [Card] atau langsung [Card]
    const cards = handData.contents ? handData.contents : handData;

    cards.forEach((card, index) => {
        const div = document.createElement('div');
        div.className = "card";
        
        if (isOpen) {
            div.innerHTML = getCardHTML(card);
            // Highlight jika dipilih
            if (isMine && selectedIndices.includes(index)) {
                div.classList.add("selected");
            }
            // Event Listener Klik
            if (isMine) {
                div.onclick = () => handleCardClick(index);
            }
        } else {
            div.classList.add("back"); // Gambar punggung kartu
            div.innerHTML = "<span>?</span>";
            // Jika fase target, kita mungkin perlu klik kartu lawan (tergantung rule)
            // Untuk sekarang, kita asumsikan klik kartu lawan hanya via UI Powerup khusus jika perlu
            // Tapi jika Haskell memperbolehkan target kartu lawan tertutup:
            if (!isMine && getPhaseTag(gameState.phase) === "ResolvePowerup") {
                 // Logic target lawan bisa ditambahkan disini
            }
        }
        container.appendChild(div);
    });
}

function getCardHTML(card) {
    // Mapping warna & simbol
    let color = (card.suit === 'Hearts' || card.suit === 'Diamonds' || card.suit === 'Red') ? 'red' : 'black';
    let suitIcon = getSuitIcon(card.suit);
    let powerText = (card.powerup !== 'Normal') ? `<br><small style='font-size:9px'>${card.powerup}</small>` : '';
    
    return `<div style="color:${color}">
                <strong>${card.rank}</strong>
                <div style="font-size:24px">${suitIcon}</div>
                ${powerText}
            </div>`;
}

function getSuitIcon(suit) {
    if (suit === 'Hearts') return '♥';
    if (suit === 'Diamonds') return '♦';
    if (suit === 'Clubs') return '♣';
    if (suit === 'Spades') return '♠';
    return '★'; // Joker
}

// --- INTERAKSI USER ---

function handleCardClick(index) {
    const phase = getPhaseTag(gameState.phase);

    // 1. Fase Discard: Klik langsung kirim ke server
    if (phase === "DiscardPhase") {
        if (confirm(`Buang kartu ke-${index + 1}?`)) {
            sendAction('discard', index);
        }
    } 
    // 2. Fase Powerup/Target: Klik untuk Select/Deselect
    else if (phase === "ResolvePowerup") {
        const pos = selectedIndices.indexOf(index);
        if (pos > -1) selectedIndices.splice(pos, 1); // Hapus seleksi
        else selectedIndices.push(index); // Tambah seleksi
        
        renderTable(); // Re-render untuk update border kuning
    }
}

function updateControls() {
    const phase = getPhaseTag(gameState.phase);
    const btnDraw = document.getElementById('btn-draw');
    const btnConfirm = document.getElementById('btn-confirm');
    const instruct = document.getElementById('instruction-text');

    // Reset Default
    btnDraw.disabled = true;
    btnConfirm.style.display = 'none';

    if (phase === "DrawPhase") {
        instruct.innerText = "Giliran Anda! Silakan Ambil Kartu.";
        btnDraw.disabled = false;
        btnDraw.innerText = "AMBIL KARTU";
    } else if (phase === "DiscardPhase") {
        instruct.innerText = "Pilih 1 kartu di tangan untuk dibuang.";
        btnDraw.innerText = "Buang Kartu (Klik Kartu)";
    } else if (phase === "ResolvePowerup") {
        instruct.innerText = `Powerup Aktif! Pilih target lalu Konfirmasi.`;
        btnConfirm.style.display = 'inline-block';
        btnConfirm.innerText = `KONFIRMASI TARGET (${selectedIndices.length})`;
    }
}

function checkPrivateInfo(myId) {
    if (gameState.privateInfo && gameState.privateInfo.length > 0) {
        gameState.privateInfo.forEach(info => {
            // info struct: [playerId, messageString]
            if (info[0] === myId) {
                // Tampilkan Alert Browser yang native (paling aman agar terbaca)
                alert("INFO RAHASIA:\n" + info[1]);
                
                // Setelah dibaca, kita idealnya hapus info agar tidak muncul terus
                // Tapi karena state dari server, kita biarkan sampai giliran ganti
            }
        });
    }
}

// --- ACTIONS KE SERVER ---

async function sendAction(type, payload) {
    let body = { actionType: type };
    if (type === 'discard') body.targetIdx = payload;
    if (type === 'target') body.targetIndices = payload;

    const res = await fetch(`${API_URL}/game/action`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body)
    });
    
    const json = await res.json();
    if (json.error) {
        alert("Server Error: " + json.error);
    } else {
        selectedIndices = []; // Reset seleksi jika sukses
        fetchState(); // Refresh segera
    }
}

function actionDraw() { sendAction('draw'); }
function actionConfirmTarget() { sendAction('target', selectedIndices); }
function resetGame() { fetch(`${API_URL}/game/reset`, { method: 'POST' }).then(fetchState); }

// --- LOGIKA OVERLAY HOTSEAT ---

function showTurnOverlay(nextPid) {
    const overlay = document.getElementById('turn-overlay');
    const nameLabel = document.getElementById('next-player-name');
    
    nameLabel.innerText = `PEMAIN ${nextPid}`;
    overlay.style.display = 'flex'; // Munculkan Overlay
    
    // Sembunyikan Game Over jika ada
    document.getElementById('game-over-overlay').style.display = 'none';
}

function confirmTurnStart() {
    // User mengklik tombol "SAYA SIAP"
    // Update activeSeat lokal agar sama dengan server
    const overlay = document.getElementById('turn-overlay');
    
    // Kita ambil ID dari teks (cara lazy) atau dari gameState yang dipending
    // Lebih aman: ambil dari gameState global (karena polling jalan terus)
    if (gameState) {
        activeSeat = gameState.currentTurn;
        overlay.style.display = 'none'; // Tutup Overlay
        selectedIndices = []; // Bersihkan seleksi sisa
        renderTable(); // Render ulang meja dengan sudut pandang baru
    }
}

function renderGameOver() {
    document.getElementById('turn-overlay').style.display = 'none';
    document.getElementById('game-over-overlay').style.display = 'flex';
}

// Helper parsing fase haskell (kadang String, kadang Object)
function getPhaseTag(phase) {
    return (typeof phase === 'object') ? phase.tag : phase;
}