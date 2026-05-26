/* =========================================
   VARIÁVEIS GLOBAIS DAS INSTÂNCIAS
========================================= */
let tsAno, tsUF, tsEscola, tsDepAdm;

/* =========================================
   PREENCHER FILTROS
========================================= */
function preencherFiltros(){
    preencherSelect("filtroAno", "ANO");
    preencherSelect("filtroUF", "estado_prova");
    preencherSelect("filtroEscola", "tipo_escola");
    preencherSelect("filtroDepAdm", "dep_adm");

    // CRUCIAL: Só transforma em TomSelect DEPOIS que o HTML já tem as opções
    iniciarTomSelects();
}

function preencherSelect(idSelect, coluna){
    let select = document.getElementById(idSelect);
    
    // Para o ano, NÃO vamos colocar a opção "Todos" no HTML
    if (idSelect === "filtroAno") {
        select.innerHTML = ''; 
    } else {
        select.innerHTML = '<option value="Todos">Todos</option>';
    }

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
   INICIALIZAR TOM SELECT
========================================= */
function iniciarTomSelects() {
    const configPadrao = {
        plugins: ['remove_button', 'clear_button'],
        maxItems: null,
        allowEmptyOption: true,
        closeAfterSelect: false
    };

    // Destrói instâncias antigas se elas existirem
    if(tsAno) tsAno.destroy();
    if(tsUF) tsUF.destroy();
    if(tsEscola) tsEscola.destroy();
    if(tsDepAdm) tsDepAdm.destroy();

    // Cria os componentes nos elementos HTML preenchidos
    tsAno = new TomSelect("#filtroAno", { ...configPadrao, placeholder: "Selecione anos" });
    tsUF = new TomSelect("#filtroUF", { ...configPadrao, placeholder: "Selecione estados" });
    tsEscola = new TomSelect("#filtroEscola", { ...configPadrao, placeholder: "Selecione escolas" });
    tsDepAdm = new TomSelect("#filtroDepAdm", { ...configPadrao, placeholder: "Selecione dependências" });

    // FORÇAR O ANO A COMEÇAR EM 2023
    // O método setValue espera um Array para selects múltiplos
    tsAno.setValue(["2023"]); 
}

/* =========================================
   ESCUTAR MUDANÇAS NOS FILTROS (API TOM SELECT)
========================================= */
function configurarEventosFiltros() {
    if(tsAno) tsAno.on('change', atualizarPainel);
    if(tsUF) tsUF.on('change', atualizarPainel);
    if(tsEscola) tsEscola.on('change', atualizarPainel);
    if(tsDepAdm) tsDepAdm.on('change', atualizarPainel);
}