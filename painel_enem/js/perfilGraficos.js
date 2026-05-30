/* =========================================================================
   PERFIL GRÁFICOS - Lógica de Visualização
========================================================================= */

function atualizarGraficosPerfil() {
    const base = perfilEstado.dadosFiltrados;

    // --- 1. GRÁFICO VERTICAL (Coluna do Meio: Raça) ---
    function desenharBarraVertical(id, coluna, titulo) {
        const dados = {};
        base.forEach(l => {
            const val = l[coluna] || "N/A";
            dados[val] = (dados[val] || 0) + (l.TOTAL_PEOPLE || 0);
        });

        const trace = [{
            x: Object.keys(dados),
            y: Object.values(dados),
            type: 'bar',
            marker: { color: '#3498db' }
        }];

        Plotly.newPlot(id, trace, {
            margin: { t: 40, b: 80, l: 40, r: 20 },
            
            xaxis: { tickangle: -45 }
        }, {responsive: true});
    }

    desenharBarraVertical('graficoRaca', 'TP_COR_RACA', 'Cor/Raça');

    // --- 2. GRÁFICO DE SEXO (Barra 100% Empilhada Horizontal com %) ---
    // Corrigido: Declaração das variáveis antes do uso
    const fem = base.reduce((soma, l) => l.TP_SEXO === 'Feminino' ? soma + l.TOTAL_PEOPLE : soma, 0);
    const masc = base.reduce((soma, l) => l.TP_SEXO === 'Masculino' ? soma + l.TOTAL_PEOPLE : soma, 0);
    const total = fem + masc;
    
    const percFem = total > 0 ? ((fem / total) * 100).toFixed(1) : 0;
    const percMasc = total > 0 ? ((masc / total) * 100).toFixed(1) : 0;

    const dataSexo = [
        { 
            x: [fem], 
            y: ['Sexo'], 
            orientation: 'h', 
            type: 'bar', 
            name: 'Feminino', 
            text: [`${percFem}%`], 
            textposition: 'inside', 
            marker: { color: '#f39c12' } 
        },
        { 
            x: [masc], 
            y: ['Sexo'], 
            orientation: 'h', 
            type: 'bar', 
            name: 'Masculino', 
            text: [`${percMasc}%`], 
            textposition: 'inside', 
            marker: { color: '#2ecc71' } 
        }
    ];

    Plotly.newPlot('graficoSexo', dataSexo, {
        barmode: 'stack',
        bargap: 0.7, // Deixa a barra mais fina
        margin: { t: 0, b: 30, l: 40, r: 20 },
        xaxis: { visible: false },
        yaxis: { visible: false },
        showlegend: true
    }, {responsive: true});

    // --- 3. GRÁFICO DE FAIXA ETÁRIA (Barra HORIZONTAL) ---
    const idades = {};
    base.forEach(l => {
        idades[l.TP_FAIXA_ETARIA] = (idades[l.TP_FAIXA_ETARIA] || 0) + l.TOTAL_PEOPLE;
    });

    const dataIdade = [{
        x: Object.values(idades), 
        y: Object.keys(idades),   
        type: 'bar',
        orientation: 'h',         
        marker: { color: '#002d62' }
    }];

    Plotly.newPlot('graficoIdade', dataIdade, {
        margin: { t: 30, b: 30, l: 120, r: 20 }, 
        
        yaxis: { autorange: 'reversed' }
    }, {responsive: true});
}