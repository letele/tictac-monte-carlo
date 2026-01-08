document.addEventListener('DOMContentLoaded', () => {
    const shinyApp = document.getElementById("shiny-app")

    const switchTab = (container_id, id) => {

        shinyApp.querySelectorAll(`#${container_id} .tab-link`).forEach(link =>
            link.classList.toggle(
                'active-tab',
                link.getAttribute('data-tab') === id
            )
        )

        shinyApp.querySelectorAll(`#${container_id} .tab-content`).forEach(page =>
            page.classList.toggle(
                'active-tab-content',
                page.getAttribute('id') === id
            )
        )
    }

    const handlerTab = (tab, id) => {
        shinyApp.querySelectorAll(`#${tab} .tab-link`).forEach(link =>
            link.addEventListener('click', () =>
                switchTab(tab, link.getAttribute('data-tab'))
            )
        )
        switchTab(tab, id)
    }

    handlerTab("methodology", "terminal")
    handlerTab("analysis", "dataset")

    const btn = document.getElementById("downloadBtn")
    const menu = document.getElementById("downloadMenu")
    const downloadBtn = document.getElementById("download_data")

    btn.addEventListener("click", function (e) {
        e.stopPropagation()
        menu.classList.toggle("open")
    })

    document.addEventListener("click", function (e) {
        if (!menu.contains(e.target) && !btn.contains(e.target)) {
            menu.classList.remove("open")
        }
    })

    if (downloadBtn) {
        downloadBtn.addEventListener("click", () => menu.classList.remove("open"))
    }
})


function renderBoard(id, board, dim = "120px", move, fs) {
    const container = document.getElementById(id)
    if (!container) return

    container.style.width = dim
    container.style.height = dim
    container.style.display = "flex"
    container.style.flexWrap = "wrap"
    const colors = { draw: 'grey', oWin: '#f4a000', xWin: '#7a3cff' }

    const text = (val, pos) => val === 0 ? pos + 1 : val === 1 ? "X" : "O"
    const color = val => val === 0 ? colors["draw"] : val === 1 ? colors["xWin"] : colors["oWin"]
    const borderStyle = side => `border-${side}: 2px solid #aaa;`
    container.innerHTML = `
    <style>
        #${id} .cell:not(:nth-of-type(3n)){ ${borderStyle("right")} }
        #${id} .cell:nth-of-type(-n + 6){ ${borderStyle("bottom")} }
           
        #${id} .cell{
            position: relative;
            font-size:${fs ? fs : '1.5em'};
            font-weight:900;
            display:flex;
            align-items: center;
            justify-content: center;
            width: 33%;
            height: 33%;
        }
        #${id} .move {
            box-shadow: inset 0 0 0 3px red;;
        }
    </style>
    ${board.map((i, j) => `
        <div 
            style="color: ${color(i)};" class="cell  ${move === j + 1 ? "move" : ""}"
        >${text(i, j)}</div>
    `).join("")}`
}

// PURE MONTE CARLO 

Shiny.addCustomMessageHandler("boardState", res => {

    const resetElements = [
        "pmc-arrow", "pmc-final-board", "pmc-heading", "pmc-table", "pmc-board"
    ]
    resetElements.forEach(i => document.getElementById(i).innerHTML = "")

    renderBoard("pmc-init-board", res.board, "8em")
})

Shiny.addCustomMessageHandler("pmsResults", res => {

    const arrow = document.getElementById("pmc-arrow")
    const colors = { draw: 'grey', oWin: '#f4a000', xWin: '#7a3cff' }

    const player = val => `
        <span style="color:  ${val === 1 ? colors["xWin"] : colors["oWin"]};">
            ${val === 1 ? "X" : "O"}
        </span>
    `

    arrow.innerHTML = `
        <div class="flex-col align-c">
            <div> <strong>${player(res.player)}</strong> moves to cell  ${res.cell} </div>
            <img src="arrow_right.svg" alt="arrow" style="height:1.5em;" />
        </div>
    `

    renderBoard("pmc-final-board", res.newBoard, "8em", res.cell)

    document.getElementById("pmc-heading").innerHTML = "Frequency Distribution Of Empty Cells"

    const { pmcResults } = res

    const columns = Object.keys(pmcResults)

    const pmcTable = document.getElementById("pmc-table")

    pmcTable.innerHTML = `
        <style>
            #pmc-table .head{
                font-weight: 600;
                text-transform: capitalize;
            }
            #pmc-table .cell{
                width: 50px;
                padding: 0.1em 0.5em;
                text-align:center;
            }
            #pmc-table .row,
            #pmc-table .body .row:not(:last-of-type){
                border-bottom: 1px solid grey;    
            }
      
            #pmc-table .row .cell:not(:last-of-type){
                border-right: 1px solid grey; 
            }
            #pmc-table .body .row:last-child{
                border-bottom: none;
            }
        </style>
        <div class="flex-col mh-0">
            <div class="flex row head">
                ${columns.map(col => `<div class="cell">${col}</div>`).join("")}
            </div>
      
            <div class="ovy-auto body">${Object.values(pmcResults)[0].map((_, i) => `
                <div class="flex row">${columns.map(col => `
                    <div class="cell">${pmcResults[col][i]}</div>`
    ).join("")}</div>
            `).join("")}</div>
        </div>
    `

    renderBoard("pmc-board", res.board, "10em", false, "2em")
    const pmcBoard = document.getElementById("pmc-board")
    const cells = pmcBoard.querySelectorAll("#pmc-board .cell")

    function getRowByCell(cellValue) {
        const index = pmcResults.cell.indexOf(cellValue)

        if (index === -1) return

        return Object.fromEntries(Object.entries(pmcResults).filter(
            ([k]) => k !== 'cell').map(([k, v]) => [k, v[index]])
        )

    }

    cells.forEach(cell => {
        const cellNumber = parseInt(cell.textContent)

        if (!isNaN(cellNumber)) {
            const barColors = {
                draw: 'grey',
                win: res.player === 1 ? '#7a3cff' : '#f4a000',
                lose: res.player === 1 ? '#f4a000' : '#7a3cff',
            }

            cell.innerHTML = `<div class="fill-div flex align-e jus-even">
            ${Object.entries(getRowByCell(cellNumber)).map(([key, value]) => `
                <div style="
                    width: 25%; height: ${100 * value}%; 
                    background: ${barColors[key]};
                "></div>
            `).join('')}</div>`
        }

    })

})


// MONTE CARLO TREE SEARCH

Shiny.addCustomMessageHandler("mctsBoardState", res => {

    const resetElements = [
        "mcts-arrow", "mcts-final-board", "mcts-heading", "mcts-table",
        "mcts-probabilities", "game-tree"
    ]
    resetElements.forEach(i => document.getElementById(i).innerHTML = "")

    renderBoard("mcts-init-board", res.board, "8em")
})

Shiny.addCustomMessageHandler("mctsResults", res => {

    const { mctsResults, player } = res

    const arrow = document.getElementById("mcts-arrow")
    const colors = { Draw: 'grey', O_win: '#f4a000', X_win: '#7a3cff' }

    const playerText = val => `
        <span style="color:  ${val === 1 ? colors["X_win"] : colors["O_win"]};">
            ${val === 1 ? "X" : "O"}
        </span>`
    arrow.innerHTML = `
        <div class="flex-col align-c">
            <div> <strong>${playerText(player)}</strong> moves to cell  ${res.prediction} </div>
            <img src="arrow_right.svg" alt="arrow" style="height:1.5em;" />
        </div>
    `

    renderBoard("mcts-final-board", res.newBoard, "8em", res.prediction)

    document.getElementById("mcts-heading").innerHTML = "Monte Carlo Tree Search Results"

    const mctsTable = document.getElementById("mcts-table")

    const columns = Object.keys(mctsResults)

    mctsTable.innerHTML = `
        <style>
            #mcts-table .head{
                font-weight: 600;
                text-transform: capitalize;
            }
            #mcts-table .cell{
                width: 50px;
                padding: 0.1em 0.5em;
                text-align:center;
            }
            #mcts-table .row,
            #mcts-table .body .row:not(:last-of-type){
                border-bottom: 1px solid grey;    
            }
      
            #mcts-table .row .cell:not(:last-of-type){
                border-right: 1px solid grey; 
            }
            #mcts-table .body .row:last-child{
                border-bottom: none;
            }
        </style>
        <div class="flex row head">
            ${columns.map(col => `<div class="cell">${col}</div>`).join("")}
        </div>
      
        <div class="ovy-auto body">${Object.values(res.mctsResults)[0].map((_, i) => `
            <div class="flex row">${columns.map(col => `
                <div class="cell">${res.mctsResults[col][i]}</div>
            `).join("")}</div>
        `).join("")}</div>
        
    `

    const tree = document.getElementById("game-tree")
    tree.innerHTML = res.treePlot ? `
        <style>
            .svg-container{
                height: 100%; 
                aspect-ratio:1/1;
            }
            .svg-container svg{
                width:100%;
                height:100%;
            }
        </style>
        <div class="svg-container">
            ${res.treePlot}
        </div>
    ` : ""

    const [xNum, xDenom] = res.probabilities[player === 1 ? "win" : "lose"] || [0, 1]
    const [oNum, oDenom] = res.probabilities[player === -1 ? "win" : "lose"] || [0, 1]
    const [dNum, dDenom] = res.probabilities["draw"] || [0, 1]

    const fractionHTML = (num, den, color,label) => num > 0  ? `
    <div class="flex align-c gap-05" style="margin-left:1em;">
        <strong style="color:${color}; width:54px;">${label}:</strong> 
        <span style="
            display:inline-flex; 
            flex-direction:column; 
            text-align:center; 
            line-height:1;"
        >
            <span>${num}</span>
            <span style="border-top:1px solid #000; padding-top:1px;">${den}</span>
        </span>
    </div>
    ` : ""

    const mctsProbabilities = document.getElementById("mcts-probabilities")
    mctsProbabilities.innerHTML = `
        <style>    
            .mcts-probs{
                border:1px solid #ccc; 
                box-shadow: 0 0 8px #00000033; 
                padding: 0.3em;
                width:150px;
            }
        </style>    
        <div  class="mcts-probs flex-col gap-05 ">
            <h4>Probabilities</h4>
            ${fractionHTML(xNum, xDenom,"#8b5cf6","X wins")}
            ${fractionHTML(oNum, oDenom,"#f97316","O wins")}
            ${fractionHTML(dNum, dDenom,"#4b5563","Draw")}
        </div>
    `
})

// Dataset

Shiny.addCustomMessageHandler("boardsData", res => {

    const { boardsData } = res
    const cells = Object.keys(boardsData)
    const rowCount = boardsData[cells[0]].length

    document.getElementById("total-rows").innerHTML = `
        Total Rows: <strong>${rowCount}</strong>
    `

    const board = document.getElementById("board-dataset")
    board.style.width = "fit-content"

    board.innerHTML = `
        <style>
            :root{
                --border-strong: #8f8f8fff;
                --border-light: #cacacaff;
            }

            #board-dataset .head{
                font-weight: 600;
                text-transform: capitalize;
                background: #eaeaea;
            }

            #board-dataset .cell{
                width: 50px;
                padding: 0.05em 0.5em;
                text-align: center;
                box-sizing: border-box;
            }
            #board-dataset .index{
                width: 32px;
                padding: 0;
                background: #f3f3f3;
                text-align: center;
            }
            #board-dataset .cell.index input[type="radio"] {
                transform: scale(0.9); 
                cursor: pointer;
            }
            #board-dataset .head .cell{
                border-right: 1px solid var(--border-strong);
                border-bottom: 1px solid var(--border-strong);
            }
            #board-dataset .head .cell:last-child{
                border-right: none;
            }
            #board-dataset .body .row .cell{
                border-right: 1px solid var(--border-light);
                border-bottom: 1px solid var(--border-light);
            }
            #board-dataset .body .row .index{
                border-right: 1px solid var(--border-strong);
                border-bottom: 1px solid var(--border-strong);
            }
            #board-dataset .body .row .cell:last-child{
                border-right: none;
            }
            #board-dataset .body .row:last-child .cell{
                border-bottom: none;
            }
        </style>

        <div class="grid-row-auto-1fr ovy-auto">
            <div class="flex row head">
                <div class="cell index"></div>
                ${cells.map(cell => `<div class="cell">${cell}</div>`).join("")}
            </div>

           <div class="ovy-auto body">
                ${Array.from({ length: rowCount }, (_, i) => {

                const rowObj = Object.fromEntries(
                    cells.map(cell => [cell, boardsData[cell][i]])
                )

                return `
                    <div class="flex row">
                        <div class="cell index flex align-c jus-c">
                        <input type="radio"
                                name="selectedRow"
                                data-row='${JSON.stringify(rowObj)}' />
                        </div>
                        ${cells.map(cell => `
                        <div class="cell">${boardsData[cell][i]}</div>
                        `).join("")}
                    </div>
                `
            }).join("")}
            </div>
        </div>
    `

    const convertToArray = (boardState) => {

        const board = Object.keys(boardState)
            .filter(key => /^[1-9]$/.test(key))
            .sort((a, b) => a - b)
            .map(key => boardState[key])

        const { PMC, MCTS } = boardState

        return { board, PMC, MCTS }
    }

    let lastSelectedRadio = null

    board.querySelectorAll('input[name="selectedRow"]').forEach(radio => {
        radio.addEventListener('click', () => {

            if (radio === lastSelectedRadio) {
                radio.checked = false
                lastSelectedRadio = null
                const resetElements = ["player_move", "pmc_value", "mcts_value", "board_view"]
                resetElements.forEach(i => document.getElementById(i).innerHTML = "")
                return
            }

            lastSelectedRadio = radio

            const rowData = convertToArray(JSON.parse(radio.dataset.row))

            const player = (rowData.board.filter(cell => cell !== 0).length % 2 !== 0) ? -1 : 1
            document.getElementById("player_move").innerHTML = `
                <span style="color:${player === -1 ? "#f97316" : "#8b5cf6"}; font-weight:900; font-size: 1.2em;
                    ">${player === -1 ? "O" : "X"}
                </span>'s turn:
            `
            document.getElementById("pmc_value").innerHTML = `
                Pure Monte Carlo: <strong>${rowData.PMC}</strong> 
            `
            document.getElementById("mcts_value").innerHTML = `
                Monte Carlo Tree Search: <strong>${rowData.MCTS}</strong> 
            `
            renderBoard("board_view", rowData.board, dim = "120px", false, "24px")
        })
    })

    const { outcome, count, rate } = res.agreeTable
    const barWidth = "150px"
    const barColors = ["#4eb8f1ff", "#fc5e5e"]

    const statsContainer = document.getElementById("stats")
    statsContainer.innerHTML = `
        <style>
            #stats svg{
                width: 100%;
                height: 100%;
            }
            .statsw70{
                width:70px;
            }
            .statsw145{
                width:145px;
            }
        </style>

        <h3 style="margin:0.5em 0">Agreement Confusion Matrix: PMC vs MCTS</h3>
        <div class="flex gap-1">
            <div>
                <div style="
                    height: 390px;
                    aspect-ratio: 7 / 5;
                ">${res.heatMap}</div>
                <div style="width:calc(390px * 7 / 5 ); padding-left:1.5em;">
                <strong>Plot 1</strong>: Confusion matrix displaying outcome differences and similarities between 
                Pure Monte Carlo (PMC) and Monte Carlo Tree Search (MCTS) 3 x 3 Tic Tac Toe predictions.
                </div>
            </div>
            <div>
                <div>Total Outcomes: <strong>${count.reduce((prev, curr) => prev + curr, 0)}</strong></div>
                ${outcome.map((i, j) => `
                    <div class="flex" style="margin: 0.5em 0;  height:40px;">
                    <div class="statsw70" style="align-self:center; text-transform: capitalize;"
                    >${i}</div>
                    <div class="flex">
                        <div style="
                            align-self:center;
                            font-weight:600;
                        "
                        >${count[j]}</div>
                        <div class="flex align-c" style="border-left: 1px solid #5f5f5f;
                            width:calc(${barWidth} * ${rate[j] / 100});
                            margin:0 0.2em 0 0.5em;
                            "
                        >
                            <div class="flex align-c" style=" 
                                width: 100%; 
                                height:80%; 
                                background:${barColors[j]};
                                border-radius: 0 3px 3px 0;
                                "
                            ></div>
                        </div>
                        <div style="align-self:center;">${rate[j]}%</div>

                    </div>
                </div>
                `).join("")}
            </div>
        </div>

        <h3 style="margin:1em 0 0.5em 0;">Move Prediction Comparison</h3>
        <div class="flex gap-1">
           <div>
                <div style="
                background:red;
                    height: 350px;
                    aspect-ratio: 7 / 4;
                ">${res.freqPlot}</div>
                <div style="width:calc(350px * 7 / 4 ); padding-left:1.5em;">
                <strong>Plot 2</strong>: Side-by-side comparison of how often each 
                algorithm selects each board position as the optimal first move.
                </div>
           </div>
            <div>
                <div class="flex" style="
                    border-bottom: 1px solid #ccc; 
                    font-weight: 600;
                    color: #5f5f5f;
                ">
                    <div class="statsw145" style="
                        text-transform:capitalize;"
                    >${Object.keys(res.statsSummary)[0]}</div>    
                    <div style="text-transform:capitalize;">${Object.keys(res.statsSummary)[1]}</div>    
                </div>
                ${res.statsSummary.statistic.map((i, j) => `
                    <div class="flex" style="margin-top: 0.5em;">
                        <div class="statsw145">${i}</div>
                        <div style="font-weight:600;">${res.statsSummary.value[j]}</div>
                    </div>
                `).join("")}
            </div>
        </div>
    `
})