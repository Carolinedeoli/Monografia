/* =========================================
   ESTADO GLOBAL DO PAINEL DE PERFIL
========================================= */
const perfilEstado = {
    dadosBrutos: [],    // Guarda a base DIM_PERFIL_UNICO completa vinda do R
    dadosFiltrados: []  // Guarda apenas as linhas que passaram pelos filtros ativos
};


/* =========================================
   1. CARREGAMENTO DA BASE UNIFICADA
========================================= */
async function iniciarPerfil() {
    try {
        console.log("Baixando base unificada DIM_PERFIL_UNICO.csv...");
        
        // Busca o arquivo gerado pelo R na sua pasta de dados
        const resposta = await fetch("dados/DIM_PERFIL_UNICO.csv");
        if (!resposta.ok) throw new Error(`Erro de rede: ${resposta.statusText}`);
        
        const textoCSV = await resposta.text();

        // Processa o CSV com o PapaParse padrão (separado por vírgula devido ao write_csv do R)
        const resultado = Papa.parse(textoCSV, {
            header: true,
            dynamicTyping: true,
            skipEmptyLines: 'greedy'
        });

        // Limpa linhas nulas e salva no estado global
        perfilEstado.dadosBrutos = resultado.data.filter(linha => linha && Object.keys(linha).length > 0);
        console.log(`✅ Base carregada! Total de combinatórias de perfil: ${perfilEstado.dadosBrutos.length}`);

        // Se a carga deu certo, monta a interface do usuário
        if (perfilEstado.dadosBrutos.length > 0) {
            preencherSelectAnoRealPerfil();
            preencherTodosOsCheckboxes();
            configurarEventosMudanca();
            
            // Executa a primeira filtragem automática na tela (Padrão: 2023)
            filtrarEAtualizarPainelPerfil();
        }

    } catch (erro) {
        console.error("Erro crítico ao inicializar o painel de perfil:", erro);
    }
}

/* =========================================
   2. RENDERIZAÇÃO DINÂMICA DOS FILTROS (HTML)
========================================= */
function preencherSelectAnoRealPerfil() {
    const selectAno = document.getElementById("filtroAnoPerfil");
    if (!selectAno) return;
    
    selectAno.innerHTML = '<option value="2022">2022</option><option value="2023" selected>2023</option>';
}

function preencherTodosOsCheckboxes() {
    // Mapeia o ID da caixinha no seu HTML com o nome exato da coluna gerada pelo R
    preencherListaDeCheckboxes("filtroUFPerfil", "estado_prova");
    preencherListaDeCheckboxes("filtroSexoPerfil", "TP_SEXO");
    preencherListaDeCheckboxes("filtroRacaPerfil", "TP_COR_RACA");
    preencherListaDeCheckboxes("filtroFaixaPerfil", "TP_FAIXA_ETARIA");
    preencherListaDeCheckboxes("filtroConclusaoPerfil", "TP_ST_CONCLUSAO");
    preencherListaDeCheckboxes("filtroEscolaPerfil", "tipo_escola");
    preencherListaDeCheckboxes("filtroDepAdmPerfil", "dep_adm");
    preencherListaDeCheckboxes("filtroTreineiroPerfil", "IN_TREINEIRO");
    preencherListaDeCheckboxes("filtroMaePerfil", "escolaridade_mae");
    preencherListaDeCheckboxes("filtroRendaPerfil", "renda_familiar");
    preencherListaDeCheckboxes("filtroInternetPerfil", "internet");
}

function preencherListaDeCheckboxes(idContainer, nomeColuna) {
    const container = document.getElementById(idContainer);
    if (!container) return;
    
    container.innerHTML = "";

    // Extrai os valores únicos daquela coluna direto da base na memória para criar as opções
    let valoresUnicos = [...new Set(perfilEstado.dadosBrutos.map(linha => {
        let val = linha[nomeColuna];
        return (val !== undefined && val !== null) ? val.toString().trim() : "";
    }))].filter(v => v !== "");

    // Ordena os filtros de forma alfabética/numérica inteligente
    valoresUnicos.sort((a, b) => a.localeCompare(b, undefined, { numeric: true, sensitivity: 'base' }));

    // Cria os elementos HTML de checkbox na tela
    valoresUnicos.forEach(valor => {
        let label = document.createElement("label");
        label.className = "filtro-checkbox-label";

        let checkbox = document.createElement("input");
        checkbox.type = "checkbox";
        checkbox.value = valor;
        checkbox.setAttribute("data-coluna", nomeColuna); // Guarda a coluna alvo no input

        label.appendChild(checkbox);
        label.appendChild(document.createTextNode(" " + valor));
        container.appendChild(label);
    });
}

/* =========================================
   3. CONFIGURAÇÃO DE EVENTOS (LISTENERS)
========================================= */
function configurarEventosMudanca() {
    // Escuta a troca de ano no Select
    const selectAno = document.getElementById("filtroAnoPerfil");
    if (selectAno) {
        selectAno.addEventListener("change", filtrarEAtualizarPainelPerfil);
    }

    // Escuta qualquer clique nos checkboxes gerados
    const wrapperFiltros = document.querySelector('.filtros-card-perfil');
    if (wrapperFiltros) {
        wrapperFiltros.addEventListener("change", function(e) {
            if (e.target.type === "checkbox") {
                filtrarEAtualizarPainelPerfil();
            }
        });
    }
}

/* =========================================
   4. MOTOR DE FILTRAGEM (EM MEMÓRIA)
========================================= */
function filtrarEAtualizarPainelPerfil() {
    // 1. Captura o ano selecionado no Select
    const selectAno = document.getElementById("filtroAnoPerfil");
    const anoSelecionado = selectAno ? selectAno.value.toString() : "2023";

    // 2. Coleta todos os checkboxes que estão ativados pelo usuário
    const checkboxesMarcados = Array.from(document.querySelectorAll('.filtros-card-perfil input[type="checkbox"]:checked'));
    
    // Agrupa os filtros ativos por coluna. Exemplo de resultado: 
    // { "TP_SEXO": ["Feminino"], "estado_prova": ["SP", "RJ"] }
    const filtrosAtivos = {};
    checkboxesMarcados.forEach(cb => {
        const coluna = cb.getAttribute("data-coluna");
        if (!filtrosAtivos[coluna]) {
            filtrosAtivos[coluna] = [];
        }
        filtrosAtivos[coluna].push(cb.value);
    });

    // 3. Aplica o cruzamento na base DIM_PERFIL_UNICO
    perfilEstado.dadosFiltrados = perfilEstado.dadosBrutos.filter(linha => {
        
        // Regra 1: A linha obrigatoriamente tem que pertencer ao ano selecionado na tela
        if (linha.ANO != anoSelecionado) return false;

        // Regra 2: Valida as colunas filtradas
        for (let coluna in filtrosAtivos) {
            let valorDaLinha = (linha[coluna] !== undefined && linha[coluna] !== null) ? linha[coluna].toString().trim() : "";
            
            // Se o texto exato daquela linha não estiver na lista de marcados do usuário, descarta a linha
            if (!filtrosAtivos[coluna].includes(valorDaLinha)) {
                return false;
            }
        }

        return true; // Passou em todos os filtros!
    });

    // 4. Envia o resultado final filtrado para calcular e atualizar a tela
    atualizarCardsPerfil();
    atualizarTabelasEsquerda();
    atualizarGraficosPerfil();
    atualizarTabelaConclusao();
}

/* =========================================
   5. ATUALIZAÇÃO DOS CARDS (MODELO DE CONTAGEM)
========================================= */
function atualizarCardsPerfil() {
    let base = perfilEstado.dadosFiltrados;

    // 1. TOTAL PARTICIPANTES: Soma da coluna TOTAL_PEOPLE (e não mais base.length)
    let totalParticipantes = base.reduce((soma, linha) => {
        return soma + (parseInt(linha.TOTAL_PEOPLE) || 0);
    }, 0);

    // 2. CONCLUINTES DO ANO: Procura o texto exato padronizado pelo R
    let concluintesAno = base.reduce((soma, linha) => {
        let val = linha.TP_ST_CONCLUSAO;
        if (val && val.toString().toLowerCase().includes("nesse ano")) {
            return soma + (parseInt(linha.TOTAL_PEOPLE) || 0);
        }
        return soma;
    }, 0);

    // 3. TREINEIROS ("Sim")
    let totalTreineiros = base.reduce((soma, linha) => {
        let val = linha.IN_TREINEIRO;
        if (val && val.toString().trim().toLowerCase() === "sim") {
            return soma + (parseInt(linha.TOTAL_PEOPLE) || 0);
        }
        return soma;
    }, 0);

    // 4. FAIXA ETÁRIA MAIS COMUM (Moda ponderada pelo TOTAL_PEOPLE)
    let contagemFaixa = {};
    base.forEach(linha => {
        let faixa = linha.TP_FAIXA_ETARIA;
        let peso = parseInt(linha.TOTAL_PEOPLE) || 0;
        
        if (faixa != null) {
            let txtFaixa = faixa.toString().trim();
            if (txtFaixa !== "" && txtFaixa !== "Não Informado") {
                contagemFaixa[txtFaixa] = (contagemFaixa[txtFaixa] || 0) + peso;
            }
        }
    });

    let faixaMaisComum = "-";
    let maior = 0;
    for (let faixa in contagemFaixa) {
        if (contagemFaixa[faixa] > maior) {
            maior = contagemFaixa[faixa];
            faixaMaisComum = faixa;
        }
    }

    // =========================================
    // INJEÇÃO DOS RESULTADOS NO HTML
    // =========================================
    // Ajuste estes getElementById caso os IDs do seu HTML sejam diferentes
    const elParticipantes = document.getElementById("cardParticipantesPerfil");
    const elUltimoAno = document.getElementById("cardUltimoAnoPerfil");
    const elTreineiros = document.getElementById("cardTreineirosPerfil");
    const elIdade = document.getElementById("cardIdadePerfil");

    if (elParticipantes) elParticipantes.innerText = totalParticipantes.toLocaleString("pt-BR");
    if (elUltimoAno) elUltimoAno.innerText = concluintesAno.toLocaleString("pt-BR");
    if (elTreineiros) elTreineiros.innerText = totalTreineiros.toLocaleString("pt-BR");
    if (elIdade) elIdade.innerText = faixaMaisComum;
}

/* =========================================
   6. LIMPAR FILTROS
========================================= */
function limparFiltrosPerfil() {
    // Seleciona todos os checkboxes dentro do menu e desmarca
    const checkboxes = document.querySelectorAll('.filtros-card-perfil input[type="checkbox"]');
    checkboxes.forEach(cb => cb.checked = false);
    
    // Roda a filtragem novamente para resetar os cards para o total do ano
    filtrarEAtualizarPainelPerfil();
}
/* =========================================
   7. CONTROLE VISUAL DO MENU DE FILTROS
========================================= */
function toggleFiltrosPerfil() {
    const menuFiltros = document.getElementById("menuFiltrosPerfil");
    
    // Se o menu estiver fechado ou sem estilo definido, ele abre
    if (menuFiltros.style.display === "none" || menuFiltros.style.display === "") {
        menuFiltros.style.display = "block"; // Ou "flex", dependendo do seu CSS original
    } else {
        // Se já estiver aberto, ele fecha
        menuFiltros.style.display = "none";
    }
}
// Inicia todo o fluxo do Painel Perfil
iniciarPerfil();