/* =========================================
   GRÁFICO INSCRITOS X PARTICIPANTES
========================================= */

function atualizarGraficoInscritos(base){

    let resumo = {};

    base.forEach(linha => {

        let ano = linha.ANO;

        if(!resumo[ano]){
            resumo[ano] = {
                inscritos: 0,
                participantes: 0
            };
        }

        resumo[ano].inscritos += Number(linha.total_inscritos || 0);
        resumo[ano].participantes += Number(linha.qtd_participantes || 0);

    });

    let anos = Object.keys(resumo).sort();

    let inscritos = anos.map(ano => resumo[ano].inscritos);
    let participantes = anos.map(ano => resumo[ano].participantes);

    let trace1 = {
        x: anos,
        y: inscritos,
        mode: "lines+markers",
        name: "Inscritos"
    };

    let trace2 = {
        x: anos,
        y: participantes,
        mode: "lines+markers",
        name: "Participantes"
    };

    let layout = {

    height: 250,

    margin: {
        t: 30,
        l: 50,
        r: 20,
        b: 40
    },

    legend: {
        orientation: "h",
        y: -0.2
    }
    };

    Plotly.newPlot(
        "graficoInscritos",
        [trace1, trace2],
        layout,
        {responsive: true}
    );
}


/* =========================================
   GRÁFICO PRESENÇA E PARTICIPAÇÃO
========================================= */

function atualizarGraficoPresenca(base){

    let resumo = {};

    base.forEach(linha => {

        let ano = linha.ANO;

        if(!resumo[ano]){
            resumo[ano] = {
                dia1: 0,
                dia2: 0,
                participantes: 0,
                redacoes: 0
            };
        }

        resumo[ano].dia1 +=
            Number(linha.qtd_presenca_dia_1 || 0);

        resumo[ano].dia2 +=
            Number(linha.qtd_presenca_dia_2 || 0);

        resumo[ano].participantes +=
            Number(linha.qtd_participantes || 0);

        resumo[ano].redacoes +=
            Number(linha.qtd_redacoes_validas || 0);

    });

    let anos = Object.keys(resumo).sort();

    let dia1 = anos.map(
        ano => resumo[ano].dia1
    );

    let dia2 = anos.map(
        ano => resumo[ano].dia2
    );

    let participantes = anos.map(
        ano => resumo[ano].participantes
    );

    let redacoes = anos.map(
        ano => resumo[ano].redacoes
    );

    let trace1 = {
        x: anos,
        y: dia1,
        mode: "lines+markers",
        name: "Presença Dia 1"
    };

    let trace2 = {
        x: anos,
        y: dia2,
        mode: "lines+markers",
        name: "Presença Dia 2"
    };

    let trace3 = {
        x: anos,
        y: participantes,
        mode: "lines+markers",
        name: "Participantes"
    };

    let trace4 = {
        x: anos,
        y: redacoes,
        mode: "lines+markers",
        name: "Redações Válidas"
    };

    let layout = {

    height: 220,

    margin: {
        t: 30,
        l: 50,
        r: 20,
        b: 40
    },

    legend: {
        orientation: "h",
        y: -0.2
    }
};
    Plotly.newPlot(
        "graficoPresenca",
        [trace1, trace2, trace3, trace4],
        layout,
        {responsive: true}
    );
}