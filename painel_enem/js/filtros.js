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
   ESCUTAR MUDANÇAS NOS FILTROS
========================================= */
function configurarEventosFiltros() {
    document.getElementById("filtroAno").addEventListener("change", atualizarPainel);
    document.getElementById("filtroUF").addEventListener("change", atualizarPainel);
    document.getElementById("filtroEscola").addEventListener("change", atualizarPainel);
    document.getElementById("filtroDepAdm").addEventListener("change", atualizarPainel);
}