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
   FILTRAR E ATUALIZAR PAINEL
========================================= */
function atualizarPainel(){
    const anos = Array.from(document.getElementById("filtroAno").selectedOptions).map(o => o.value);

    const ufs = Array.from(document.getElementById("filtroUF").selectedOptions).map(o => o.value);

    const escolas = Array.from(document.getElementById("filtroEscola").selectedOptions).map(o => o.value);

    const deps = Array.from(document.getElementById("filtroDepAdm").selectedOptions).map(o => o.value);
    // Filtro Geral (cards normais dependem do Ano selecionado)
    let dadosFiltradosGeral = dados.filter(linha => {
        let bateAno =
        anos.length === 0 ||
        anos.includes("Todos") ||
        anos.includes(linha.ANO?.toString());

        let bateUF =
        ufs.length === 0 ||
        ufs.includes("Todos") ||
        ufs.includes(linha.estado_prova);

        let bateEscola =
        escolas.length === 0 ||
        escolas.includes("Todos") ||
        escolas.includes(linha.tipo_escola);

        let bateDep =
        deps.length === 0 ||
        deps.includes("Todos") ||
        deps.includes(linha.dep_adm);
        return bateAno && bateUF && bateEscola && bateDep;
    });

    // Filtro para Média (Ignora o select de Ano, mantendo o histórico)
    let dadosParaMedia = dados.filter(linha => {
        let bateUF =
        ufs.length === 0 ||
        ufs.includes("Todos") ||
        ufs.includes(linha.estado_prova);

        let bateEscola =
        escolas.length === 0 ||
        escolas.includes("Todos") ||
        escolas.includes(linha.tipo_escola);

        let bateDep =
        deps.length === 0 ||
        deps.includes("Todos") ||
        deps.includes(linha.dep_adm);
        return bateUF && bateEscola && bateDep;
    });

    // Filtro da Redação (A tabela da redação obedece a todos os filtros, incluindo o Ano fixado)
    let dadosFiltradosRedacao = dadosRedacao.filter(linha => {
        let bateAno =
        anos.length === 0 ||
        anos.includes("Todos") ||
        anos.includes(linha.ANO?.toString());

        let bateUF =
        ufs.length === 0 ||
        ufs.includes("Todos") ||
        ufs.includes(linha.estado_prova);

        let bateEscola =
        escolas.length === 0 ||
        escolas.includes("Todos") ||
        escolas.includes(linha.tipo_escola);

        let bateDep =
        deps.length === 0 ||
        deps.includes("Todos") ||
        deps.includes(linha.dep_adm);
        return bateAno && bateUF && bateEscola && bateDep;
    });

    atualizarCards(dadosFiltradosGeral, dadosParaMedia);
    atualizarTabelaRedacao(dadosFiltradosRedacao);
    atualizarGraficoInscritos(dadosParaMedia);
    atualizarGraficoPresenca(dadosParaMedia);
}


const btnFiltros = document.getElementById("btnFiltros");
const painelFiltros = document.getElementById("painelFiltros");

btnFiltros.addEventListener("click", () => {
    painelFiltros.classList.toggle("fechado");
});

