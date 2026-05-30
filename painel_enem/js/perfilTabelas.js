function atualizarTabelasEsquerda() {
    const base = perfilEstado.dadosFiltrados;
    console.log("Atualizando tabelas com:", base.length, "linhas.");

    function gerarTabela(idContainer, coluna, titulo) {
    const container = document.getElementById(idContainer);
    if (!container) return;

    const contagem = {};
    let totalGeral = 0;

    perfilEstado.dadosFiltrados.forEach(linha => {
        const val = linha[coluna] || "Não Informado";
        const peso = parseInt(linha.TOTAL_PEOPLE) || 0;
        contagem[val] = (contagem[val] || 0) + peso;
        totalGeral += peso;
    });

    // 🌟 AQUI USAMOS A CLASSE: tabela-estilo-painel
    let html = `<table class="tabela-estilo-painel">
                  <thead>
                    <tr>
                        <th>${titulo}</th>
                        <th>% de Participantes</th>
                    </tr>
                  </thead>
                  <tbody>`;
    
    for (let key in contagem) {
        let perc = totalGeral > 0 ? ((contagem[key] / totalGeral) * 100).toFixed(1) : 0;
        html += `<tr>
                    <td>${key}</td>
                    <td>${perc}%</td>
                 </tr>`;
    }
    html += `</tbody></table>`;
    container.innerHTML = html;
}

    gerarTabela("tabelaEscola", "tipo_escola", "Escola");
    gerarTabela("tabelaDepAdm", "dep_adm", "Dependência");
}

function atualizarTabelaConclusao() {
    const base = perfilEstado.dadosFiltrados;
    const container = document.getElementById('tabelaConclusao');
    
    const dados = {};
    let totalGeral = 0;

    base.forEach(l => {
        const val = l.TP_ST_CONCLUSAO || "Não Informado";
        const peso = parseInt(l.TOTAL_PEOPLE) || 0;
        dados[val] = (dados[val] || 0) + peso;
        totalGeral += peso;
    });

    let html = `<table class="tabela-estilo-painel">
                  <thead><tr><th>Situação</th><th>Nº de Participantes</th><th>% de Participantes</th></tr></thead>
                  <tbody>`;
    
    for (let key in dados) {
        let qtd = dados[key];
        let perc = totalGeral > 0 ? ((qtd / totalGeral) * 100).toFixed(1) : 0;
        html += `<tr>
                    <td>${key}</td>
                    <td>${qtd.toLocaleString('pt-BR')}</td>
                    <td>${perc}%</td>
                 </tr>`;
    }
    html += `</tbody></table>`;
    container.innerHTML = html;
}