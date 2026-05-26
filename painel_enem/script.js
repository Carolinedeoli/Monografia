/* =========================================
   BASES GLOBAIS
========================================= */
let dados = [];
let dadosRedacao = [];

/* =========================================
   INICIAR CARREGAMENTO
========================================= */
carregarGeral();

/* =========================================
   FUNÇÃO LIMPAR DADOS
========================================= */
function limparDados(base){
    return base.map(linha => {
        let linhaLimpa = {};
        for(let chave in linha){
            let chaveLimpa = chave.replace(/\"/g, "").trim();
            let valorLimpo = linha[chave] ? String(linha[chave]).replace(/\"/g, "").trim() : "";
            linhaLimpa[chaveLimpa] = valorLimpo;
        }
        return linhaLimpa;
    });
}

/* =========================================
   CARREGAR BASE GERAL
========================================= */
function carregarGeral(){
    Papa.parse("dados/GERAL.csv", {
        download: true,
        header: true,
        skipEmptyLines: true,
        complete: function(resultado){
            dados = limparDados(resultado.data);
            console.log("Base Geral Carregada!");
            carregarRedacao();
        },
        error: function(erro) {
            console.error("Erro ao carregar GERAL.csv:", erro);
        }
    });
}

/* =========================================
   CARREGAR BASE REDAÇÃO
========================================= */
function carregarRedacao(){
    Papa.parse("dados/REDAÇÃO.csv", {
        download: true,
        header: true,
        skipEmptyLines: true,
        complete: function(resultado){
            dadosRedacao = limparDados(resultado.data);
            console.log("Base Redação Carregada!");

            // 1. Inicializa os selects do HTML
            preencherFiltros();
            
            // 2. Pré-seleciona o último ano
            definirUltimoAnoComoPadrao();

            // 3. Atualiza os cards e reconstrói a tabela
            atualizarPainel();
            configurarEventosFiltros();
        },
        error: function(erro) {
            console.error("Erro ao carregar REDAÇÃO.csv:", erro);
        }
    });
}

/* =========================================
   PREENCHER FILTROS
========================================= */
function preencherFiltros(){
    preencherSelect("filtroAno", "ANO");
    preencherSelect("filtroUF", "estado_prova");
    preencherSelect("filtroEscola", "tipo_escola");
    preencherSelect("filtroDepAdm", "dep_adm");
}

function preencherSelect(idSelect, coluna){
    let select = document.getElementById(idSelect);
    select.innerHTML = select.options[0].outerHTML;

    let valores = [...new Set(dados.map(linha => linha[coluna]))]
                    .filter(valor => valor !== undefined && valor !== null && valor.toString().trim() !== "");

    valores.sort();

    valores.forEach(valor => {
        let option = document.createElement("option");
        option.value = valor;
        option.text = valor;
        select.appendChild(option);
    });
}

/* =========================================
   FIXAR O ÚLTIMO ANO DISPONÍVEL
========================================= */
function definirUltimoAnoComoPadrao() {
    let selectAno = document.getElementById("filtroAno");
    let anos = [...new Set(dados.map(linha => linha.ANO))].filter(a => a).sort();

    if (anos.length > 0) {
        let ultimoAno = anos[anos.length - 1];
        selectAno.value = ultimoAno; 
    }
}

/* =========================================
   ESCUTAR MUDANÇAS NOS FILTROS
========================================= */
function configurarEventosFiltros() {
    document.getElementById("filtroAno").addEventListener("change", atualizarPainel);
    document.getElementById("filtroUF").addEventListener("change", atualizarPainel);
    document.getElementById("filtroEscola").addEventListener("change", atualizarPainel);
    document.getElementById("filtroDepAdm").addEventListener("change", atualizarPainel);
}

/* =========================================
   FILTRAR E ATUALIZAR PAINEL
========================================= */
function atualizarPainel(){
    const ano = document.getElementById("filtroAno").value.trim();
    const uf = document.getElementById("filtroUF").value.trim();
    const escola = document.getElementById("filtroEscola").value.trim();
    const depAdm = document.getElementById("filtroDepAdm").value.trim();

    // Filtro Geral (cards normais dependem do Ano selecionado)
    let dadosFiltradosGeral = dados.filter(linha => {
        let bateAno = (ano === "Todos" || (linha.ANO && linha.ANO.toString() === ano));
        let bateUF = (uf === "Todos" || (linha.estado_prova && linha.estado_prova === uf));
        let bateEscola = (escola === "Todos" || (linha.tipo_escola && linha.tipo_escola === escola));
        let bateDep = (depAdm === "Todos" || (linha.dep_adm && linha.dep_adm === depAdm));
        return bateAno && bateUF && bateEscola && bateDep;
    });

    // Filtro para Média (Ignora o select de Ano, mantendo o histórico)
    let dadosParaMedia = dados.filter(linha => {
        let bateUF = (uf === "Todos" || (linha.estado_prova && linha.estado_prova === uf));
        let bateEscola = (escola === "Todos" || (linha.tipo_escola && linha.tipo_escola === escola));
        let bateDep = (depAdm === "Todos" || (linha.dep_adm && linha.dep_adm === depAdm));
        return bateUF && bateEscola && bateDep;
    });

    // Filtro da Redação (A tabela da redação obedece a todos os filtros, incluindo o Ano fixado)
    let dadosFiltradosRedacao = dadosRedacao.filter(linha => {
        let bateAno = (ano === "Todos" || (linha.ANO && linha.ANO.toString() === ano));
        let bateUF = (uf === "Todos" || (linha.estado_prova && linha.estado_prova === uf));
        let bateEscola = (escola === "Todos" || (linha.tipo_escola && linha.tipo_escola === escola));
        let bateDep = (depAdm === "Todos" || (linha.dep_adm && linha.dep_adm === depAdm));
        return bateAno && bateUF && bateEscola && bateDep;
    });

    atualizarCards(dadosFiltradosGeral, dadosParaMedia);
    atualizarTabelaRedacao(dadosFiltradosRedacao);
}

/* =========================================
   ATUALIZAR CARDS
========================================= */
function atualizarCards(base, baseParaMedia){
    let inscritos = 0;
    let participantes = 0;
    let redacoesValidas = 0;
    let presentesDia1 = 0;

    let limparNumero = (val) => {
        if (!val) return 0;
        let apenasDigitos = val.toString().replace(/[^\d]/g, "").trim();
        return Number(apenasDigitos) || 0;
    };

    base.forEach(linha => {

    inscritos +=
        limparNumero(linha.total_inscritos);

    participantes +=
        limparNumero(linha.qtd_participantes);

    redacoesValidas +=
        limparNumero(linha.qtd_redacoes_validas);

    presentesDia1 +=
        limparNumero(linha.qtd_presenca_dia_1);
});

    let percentParticipacao = inscritos > 0 ? (participantes / inscritos) * 100 : 0;
    let percentRedacoes =
    presentesDia1 > 0
    ? (redacoesValidas / presentesDia1) * 100
    : 0;

    // Cálculo da média histórica de participantes por ano
    let participantesTotalMedia = 0;
    baseParaMedia.forEach(linha => {
        participantesTotalMedia += limparNumero(linha.qtd_participantes);
    });
    let anosUnicos = [...new Set(baseParaMedia.map(l => l.ANO))].filter(a => a);
    let qtdAnos = anosUnicos.length || 1;
    let mediaAnos = participantesTotalMedia / qtdAnos;

    document.getElementById("totalInscritos").innerText = inscritos.toLocaleString('pt-BR');
    document.getElementById("totalParticipantes").innerText = participantes.toLocaleString('pt-BR');
    document.getElementById("percentParticipacao").innerText = percentParticipacao.toFixed(2) + "%";
    document.getElementById("percentRedacoesValidas").innerText = percentRedacoes.toFixed(2) + "%";
    document.getElementById("mediaParticipantesAno").innerText = Math.round(mediaAnos).toLocaleString('pt-BR');
}

/* =========================================
   ATUALIZAR TABELA REDAÇÃO (Corrigido e Blindado)
========================================= */
function atualizarTabelaRedacao(baseFiltrada){
    let tabela = document.getElementById("tabelaRedacao");
    if (!tabela) return;

    let tbody = tabela.querySelector("tbody");
    if(!tbody) {
        tbody = document.createElement("tbody");
        tabela.appendChild(tbody);
    }
    
    tbody.innerHTML = ""; // Limpa linhas anteriores

    let resumo = {};
    let totalPessoasFiltro = 0;

    baseFiltrada.forEach(linha => {
        let status = linha.motivo_status;
        if (!status) return;

        // Garante que o valor se transforme em texto antes de limpar caracteres especiais
        let valorBruto = linha.numero_pessoas ? String(linha.numero_pessoas) : "0";
        let pessoas = Number(valorBruto.replace(/[^\d]/g, "")) || 0;

        if (!resumo[status]) {
            resumo[status] = 0;
        }
        resumo[status] += pessoas;
        totalPessoasFiltro += pessoas;
    });

    // Se nenhuma linha corresponder ao filtro, mostra um aviso amigável na tabela
    if (totalPessoasFiltro === 0) {
        tbody.innerHTML = `<tr><td colspan="3" style="text-align:center; color:#777;">Nenhum registro encontrado para este filtro.</td></tr>`;
        return;
    }

    Object.keys(resumo).forEach(status => {
        let quantidade = resumo[status];
        let porcentagem = totalPessoasFiltro > 0 ? (quantidade / totalPessoasFiltro) * 100 : 0;

        let tr = document.createElement("tr");
        tr.innerHTML = `
            <td>${status}</td>
            <td>${quantidade.toLocaleString('pt-BR')}</td>
            <td>${porcentagem.toFixed(2)}%</td>
        `;
        tbody.appendChild(tr);
    });
}