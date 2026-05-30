/* =========================================
   ATUALIZAR GRÁFICOS PERFIL
========================================= */
function atualizarGraficosPerfil() {
    let base = perfilEstado.dadosFiltrados;

    // Se a base estiver vazia, exibe uma mensagem nos containers
    if (!base || base.length === 0) {
        const mensagem = "<div style='padding:20px; text-align:center; color:#64748b;'>Nenhum dado encontrado para os filtros selecionados.</div>";
        document.getElementById("graficoRacaPerfil").innerHTML = mensagem;
        document.getElementById("graficoFaixaPerfil").innerHTML = mensagem;
        document.getElementById("graficoConclusaoPerfil").innerHTML = mensagem;
        document.getElementById("graficoSexoPerfil").innerHTML = mensagem;
        return;
    }

    // Objetos para armazenar as contagens agregadas
    let contagemRaca = {};
    let contagemFaixa = {};
    let contagemConclusao = {};
    let contagemSexo = {};

    // Agregação dos dados
    base.forEach(linha => {
        if (linha.TP_COR_RACA) contagemRaca[linha.TP_COR_RACA] = (contagemRaca[linha.TP_COR_RACA] || 0) + 1;
        if (linha.TP_FAIXA_ETARIA) contagemFaixa[linha.TP_FAIXA_ETARIA] = (contagemFaixa[linha.TP_FAIXA_ETARIA] || 0) + 1;
        if (linha.TP_ST_CONCLUSAO) contagemConclusao[linha.TP_ST_CONCLUSAO] = (contagemConclusao[linha.TP_ST_CONCLUSAO] || 0) + 1;
        if (linha.TP_SEXO) contagemSexo[linha.TP_SEXO] = (contagemSexo[linha.TP_SEXO] || 0) + 1;
    });

    const configLayoutBase = {
        font: { family: 'Arial, sans-serif' },
        margin: { t: 20, r: 20, b: 40, l: 60 },
        paper_bgcolor: 'rgba(0,0,0,0)',
        plot_bgcolor: 'rgba(0,0,0,0)',
        showlegend: true
    };

    const configPlotly = { responsive: true, displayModeBar: false };

    // 1. GRAFICO RAÇA
    Plotly.newPlot('graficoRacaPerfil', [{
        type: 'bar', x: Object.values(contagemRaca), y: Object.keys(contagemRaca), orientation: 'h', marker: { color: '#002d62' }
    }], Object.assign({}, configLayoutBase, { yaxis: { automargin: true } }), configPlotly);

    // 2. GRAFICO FAIXA ETÁRIA
    Plotly.newPlot('graficoFaixaPerfil', [{
        type: 'bar', x: Object.keys(contagemFaixa), y: Object.values(contagemFaixa), marker: { color: '#3b82f6' }
    }], Object.assign({}, configLayoutBase, { xaxis: { type: 'category', automargin: true } }), configPlotly);

    // 3. GRAFICO CONCLUSÃO
    Plotly.newPlot('graficoConclusaoPerfil', [{
        type: 'pie', labels: Object.keys(contagemConclusao), values: Object.values(contagemConclusao), hole: 0.4, textinfo: 'percent'
    }], Object.assign({}, configLayoutBase, { margin: { t: 10, r: 10, b: 10, l: 10 }, legend: { orientation: 'h', y: -0.1 } }), configPlotly);

    // 4. GRAFICO SEXO
    Plotly.newPlot('graficoSexoPerfil', [{
        type: 'pie', labels: Object.keys(contagemSexo), values: Object.values(contagemSexo), marker: { colors: ['#ec4899', '#1d4ed8'] }, textinfo: 'label+percent'
    }], Object.assign({}, configLayoutBase, { margin: { t: 10, r: 10, b: 10, l: 10 }, showlegend: false }), configPlotly);
}