using StatsPlots
using Random
using Statistics


mutable struct Agent
    strategy::String
    money::Float64
    sharesQuantity  #   株式保有部数リスト
    sharesRetainedLine  #   株式保有金額のリスト
    total_assets_log    #   資産総額の履歴
    params  #   パラメータのリスト  
    #   ファンダメンタルズ戦略をとる場合 [ポートフォリオに占める預金の割合の目標, 企業評価/時価総額　の、買いの閾値になる倍率, 企業価値/時価総額　の、売りの閾値になる倍率, ポートフォリオ分散度合い(1以上。大きい値の時ほど分散させる)]
    #   インデックス戦略をとる場合      [ポートフォリオに占める預金の割合の目標, 1期のタイムスケール, α5, α4, α3, α2, α1, α0, β1, β2, γ, ポートフォリオ分散度合い(1以上。大きい値の時ほど分散させる)]
    fundamentals    #   企業評価のリスト    ファンダメンタルズ戦略をとる場合しか使わないが、更新はインデックス戦略をとるときでも続ける
    portfolio_target#   ポートフォリオ配分目標。[預金,株式]
    purchase_cost
    performance::Float64    #   運用成績
end
mutable struct Firm
    stockPrice::Float64     #   株価
    stockQuantity::Float64  #   株式発行部数
    stockPriceLog   #   価格の記録
    stockQuantityLog    #   発行部数の記録
    marketCapitalization::Float64   #   時価総額
    hiddenCorporateValue::Float64   #   企業価値
    buy_offers  #   買い注文 [(価格(float), 量(float), (エージェントインデックス(integer)),,,,,]
    sell_offers #   売り注文 [(価格(float), 量(float), (エージェントインデックス(integer)),,,,,]
end
function update_hiddenCorporateValue(firms)
    σ_r, σ, μ = 0.05, 2.0, log(100.0)
    σ_p = sqrt(σ^2 + σ_r^2)
    for firm in firms
        f = firm.hiddenCorporateValue
        firm.hiddenCorporateValue = exp((log(f) + σ_r*randn())*σ/σ_p + μ*(1 - σ/σ_p))
    end
end
function update_estimate_corporateValue(agents, firms)
    for agent in agents
        for (i, estimated_value) in enumerate(agent.fundamentals)
            agent.fundamentals[i] = (0.05*firms[i].hiddenCorporateValue + 0.95*estimated_value) * exp(0.1*randn())
        end
    end
end
function update_portfolio_target(agents)
    for agent in agents
        if agent.total_assets_log[end] <= 0
            agent.portfolio_target[1:2] = [0,0]
            continue
        end
        agent.portfolio_target[1] = agent.total_assets_log[end] * agent.params[1]
        agent.portfolio_target[2] = agent.total_assets_log[end] * (1 - agent.params[1])
    end
end
function cal_total_asset(agents)
    for agent in agents
        push!(agent.total_assets_log, sum(agent.sharesRetainedLine) + agent.money)
    end
end
function cal_sharesRetainedLine(agents, firms)
    for agent in agents
        for i = 1:size(agent.sharesQuantity)[1]
            agent.sharesRetainedLine[i] = firms[i].stockPrice*agent.sharesQuantity[i]
        end
    end
end
function fundamentals_trade_offer(agent, firms, j, all_marketCap_sum)
    lst = []
    marketCap, mean_amp = 0.0, 0.0
    for (i, firm) in enumerate(firms)
        marketCap = firm.marketCapitalization
        fundamentals = agent.fundamentals[i]
        push!(lst, (marketCap/fundamentals, i))    #   marketCap/fundamentalsが小さいほど買いたい、大きいほど売りたい
        mean_amp += marketCap/fundamentals
    end
    mean_amp /= size(firms)[1]
    sell, buy = [], []
    #   全部売り候補に入れて、ポートフォリオ配分目標に至るまで買い候補に入れて、その差量を本チャンの売買リストに加える
    #   持ってる株をすべて仮売り。
    for (i,q) in enumerate(agent.sharesQuantity)
        if q > 0.0
            p = 0.5*mean_amp*agent.fundamentals[i]/firms[i].stockQuantity + 0.5*firms[i].stockPrice
            push!(sell, (i, p, q))
        end
    end
    #   ポートフォリオ配分目標に達するまで仮買い。
    sort!(lst)
    allocations_lst = [0.0 for _ = 1:size(firms)[1]]
    counter = 0
    while counter < Integer(agent.params[end])
        for k in 1:size(lst)[1]
            _, i = lst[k]
            if rand()*(1.0 - k/size(lst)[1]) < firms[i].marketCapitalization/all_marketCap_sum   #   規模の大きい企業ほど買いやすいように、割安判断をした企業ほど買いやすいようにする
                allocations_lst[i] += agent.portfolio_target[2]/agent.params[end]
                counter += 1
                if counter == Integer(agent.params[end])
                    break
                end
            end
        end
    end
    for k in 1:size(lst)[1]
        _, i = lst[k]
        if allocations_lst[i] == 0.0
            continue
        end
        p = 0.5*mean_amp*agent.fundamentals[i]/firms[i].stockQuantity + 0.5*firms[i].stockPrice
        q = allocations_lst[i]/p
        push!(buy, (i, p, q))
    end
    #   注文の決定
    going_to_buy_price = 0.0
    for (ib, pb, qb) in sell
        q, p = qb, pb
        for (is, ps, qs) in buy
            if ib == is
                q -= qs
                if q < 0.0
                    p = ps
                end
                break
            end
        end
        if q > 0
            push!(firms[ib].sell_offers, (p, q, j))
        elseif q < 0
            going_to_buy_price += p*(-q)
            if going_to_buy_price > agent.money
                continue
            end
            push!(firms[ib].buy_offers, (p, -q, j))
        end
    end
end
function chart_trade_offer(agent, firms, j)
    span = agent.params[2]
    if size(firms[1].stockPriceLog)[1] < span*6
        return nothing
    end
    α5, α4, α3, α2, α1 = agent.params[3:7]
    α0 = agent.params[8]
    β1, β2 = agent.params[9:10]                #   β1<β2 の条件を追加する
    sell, buy = [], []
    #   売り
    for (i, q) in enumerate(agent.sharesQuantity)
        if q == 0.0
            continue
        end
        x1, x2, x3, x4, x5, x6 = firms[i].stockPriceLog[end-5:end]
        p = α0 + α1*(x2-x1)/x1 + α2*(x3-x2)/x2 + α3*(x4-x3)/x3 + α4*(x5-x4)/x4 + α5*(x6-x5)/x5
        if p < β1
            push!(sell, (p,i))
        end
    end
    marketCap_sum = 0.0
    sell_2 = []
    for (p, i) in sell
        if agent.money < 0.0
            p = 0.0
        end
        price = (1+p)*firms[i].stockPrice
        quantity = agent.sharesQuantity[i]
        if price > 0.0 && quantity > 0.0
            push!(sell_2, (p, i))
            marketCap_sum += firms[i].marketCapitalization
        end
    end
    going_to_sell_price = 0.0
    for (p, i) in sell_2
        if agent.money < 0.0 && p > 0.0
            p = 0.0
        end
        price = (1+p)*firms[i].stockPrice
        quantity = agent.sharesQuantity[i]
        if rand() < firms[i].marketCapitalization/marketCap_sum
            going_to_sell_price += price * quantity
            push!(firms[i].sell_offers, (price, quantity, j))
        end
    end
    #   買い
    for (i, firm) in enumerate(firms)
        x1, x2, x3, x4, x5, x6 = firm.stockPriceLog[end-5:end]
        p = α0 + α1*(x2-x1)/x1 + α2*(x3-x2)/x2 + α3*(x4-x3)/x3 + α4*(x5-x4)/x4 + α5*(x6-x5)/x5
        if p > β2
            push!(buy, (p,i))
        end
    end
    sort!(buy)
    if size(buy)[1] > Integer(agent.params[end])
        buy = buy[end-Integer(agent.params[end])+1:end]
    end
    marketCap_sum = 0.0
    for (_, i) in buy
        marketCap_sum += firms[i].marketCapitalization
    end
    allocations_lst = [0.0 for _ = 1:size(firms)[1]]
    buy_target = agent.money + going_to_sell_price - agent.portfolio_target[1]
    if buy_target <= 0.0
        return nothing
    end
    for (_, i) in buy
        allocations_lst[i] = buy_target*firms[i].marketCapitalization/marketCap_sum
    end
    for (p, i) in buy
        price = (1+p)*firms[i].stockPrice
        if price <= 0.0
            continue
        end
        quantity = allocations_lst[i]/price
        push!(firms[i].buy_offers, (price, quantity, j))
    end
end
function trade_offer(agents, firms, all_marketCap_sum)
    for firm in firms
        firm.buy_offers, firm.sell_offers = [], []
    end
    for (j, agent) in enumerate(agents)
        if agent.strategy == "fundamentals"
            fundamentals_trade_offer(agent, firms, j, all_marketCap_sum)
        elseif agent.strategy == "chart"
            chart_trade_offer(agent, firms, j)
        end
    end
end
function cal_performance(agents, income)
    for agent in agents
        agent.performance = (agent.total_assets_log[end] - agent.total_assets_log[end-1] - income)/agent.total_assets_log[end-1]
    end
end
function trade_matching(agents, firms)
    for (i, firm) in enumerate(firms)
        buy, sell = firm.buy_offers, firm.sell_offers
        sort!(buy)
        sort!(sell, rev=true)
        selling_p, selling_q, buying_p, buying_q = 0.0, 0.0, 0.0, 0.0
        trading_p = firm.stockPrice
        pb, qb, jb, ps, qs, js = 0.0, 0.0, -1, 0.0, 0.0, -1
        while buying_p >= selling_p
            if size(buy)[1] == 0 || size(sell)[1] == 0
                break
            end
            if buying_q == 0.0
                pb, qb, jb = pop!(buy)
                buying_q = qb
                buying_p = pb
            end
            if selling_q == 0.0
                ps, qs, js = pop!(sell)
                selling_q = qs
                selling_p = ps
            end
            if buying_p < selling_p
                break
            end
            trading_q = min(buying_q, selling_q)
            buying_q -= trading_q
            selling_q -= trading_q
            trading_p = (pb + ps)/2
            if jb < 0   #   企業自身からのオファー
                firms[-jb].stockQuantity -= trading_q
            else
                agents[jb].purchase_cost[i] = (agents[jb].purchase_cost[i]*agents[jb].sharesQuantity[i] + trading_p*trading_q)/(agents[jb].sharesQuantity[i] + trading_q)
                agents[jb].sharesQuantity[i] += trading_q
                agents[jb].money -= trading_p*trading_q
            end
            if js < 0
                firms[-js].stockQuantity += trading_q
            else
                agents[js].sharesQuantity[i] -= trading_q
                tax, tax_rate = 0.0, 0.2
                if agents[js].purchase_cost[i] < trading_p
                    tax = tax_rate*trading_q*(trading_p - agents[js].purchase_cost[i])
                end
                agents[js].money += trading_p*trading_q - tax
            end
        end
        firm.stockPrice = trading_p
        push!(firm.stockPriceLog, trading_p)
        push!(firm.stockQuantityLog, firm.stockQuantity)
    end
end
function update_strategy(agents)
    if size(agents[1].total_assets_log)[1] < 10
        return nothing
    end
    A = Integer(size(agents)[1])
    for agent in agents
        teacher = rand(1:A)
        t0, t1 = agent.total_assets_log[end-9], agent.total_assets_log[end]
        if rand() < 0.01 - 0.01*(t1 - t0)/t0
            new_strategy = deepcopy(agents[teacher].strategy)
            agent.strategy = new_strategy
            new_params = append!([agent.params[1]], agents[teacher].params[2:end])
            agent.params = new_params
        end
        if rand() < 0.001
            if agent.strategy == "chart"
                agent.strategy = "fundamentals"
                agent.params = [0.9, 5]
            else
                agent.strategy = "chart"
                agent.params = [0.9, 1, randn(), randn(), randn(), randn(), randn(), 0.0, -0.01, 0.01, 0.5, 5]
            end
        end
    end
end
function update_params(agents)
    for agent in agents
        agent.params[1] += 0.01*randn()
        agent.params[1] = abs(agent.params[1])
        if agent.params[1] > 1
            agent.params[1] = 1 - agent.params[1] % 1
        end
        agent.params[end] += rand(-1:1)
        if agent.params[end] <= 0
            agent.params[end] = 1
        end
        if agent.strategy == "chart"
            if rand() < 0.01
                agent.params[2] += rand(-1:1) #   タイムスケール
                if agent.params[2] <= 0
                    agent.params[2] = 1
                end
            end
            agent.params[3:7] += 0.01*randn(5)
            agent.params[3:7] = (agent.params[3:7] .- sum(agent.params[3:7])/5)/(std(agent.params[3:7])*sqrt(5))
            agent.params[8] += 0.01*randn()
            agent.params[9:10] += 0.01*randn(2)
            if agent.params[9] > agent.params[10]
                agent.params[9], agent.params[10] = agent.params[10], agent.params[9]
            end
            agent.params[11] += 0.01*randn()
            if agent.params[11] < 0
                agent.params[11] = abs(agent.params[11] % 1)
            elseif agent.params[11] > 1
                agent.params[11] = 1 - agent.params[11] % 1
            end
        end
    end
end
function update_marketCapitalization(firms)
    for firm in firms
        firm.marketCapitalization = firm.stockPrice*firm.stockQuantity
    end
end
function get_income(agents, income)
    for agent in agents
        agent.money += income
    end
end
function get_dividend(agents, firms)
    Π = 0.2
    payout_ratio = 0.35
    for agent in agents
        for (i, q) in enumerate(agent.sharesQuantity)
            if q > 0.0
                dividend = q/firms[i].stockQuantity * Π*payout_ratio*firms[i].hiddenCorporateValue
                agent.money += dividend
            end
        end
    end
end
function cal_all_marketCap_sum(firms)
    all_marketCap_sum = 0.0
    for firm in firms
        all_marketCap_sum += firm.marketCapitalization
    end
    return all_marketCap_sum
end
function share_buy_back_and_share_issue_offer(firms)
    for (i, firm) in enumerate(firms)
        t = rand()
        if t < 0.1
            quantity = 0.05*firm.stockQuantity
            price = 0.95*firm.stockPrice
            push!(firm.buy_offers, (price, quantity, -i))
        elseif t > 0.9
            quantity = 0.05*firm.stockQuantity
            price = 1.05*firm.stockPrice
            push!(firm.sell_offers, (price, quantity, -i))
        end
    end
end
function run_one_term(agents, firms, income)
    all_marketCap_sum = cal_all_marketCap_sum(firms)
    update_hiddenCorporateValue(firms)
    update_estimate_corporateValue(agents, firms)
    trade_offer(agents, firms, all_marketCap_sum)
    share_buy_back_and_share_issue_offer(firms)
    trade_matching(agents, firms)
    get_dividend(agents, firms)
    cal_sharesRetainedLine(agents, firms)
    cal_total_asset(agents)
    cal_performance(agents, income)
    get_income(agents, income)
    update_strategy(agents)
    update_params(agents)
    update_portfolio_target(agents)
    update_marketCapitalization(firms)
end
function f(t)
    return 2^(0.01*t)
end




N, M = 10^2, 10^1 #   エージェント数, 株式会社数
init_stock_quantity, init_stock_price = 100.0, 1.0
init_money = init_stock_quantity*init_stock_price*M/N * 9.0

agents = [
    Agent(
        "chart",
        init_money,
        [init_stock_quantity/N for _ = 1:M],
        [init_stock_quantity*init_stock_price/N for _ = 1:M],
        [init_money/0.9],
        [0.9, 1, randn(), randn(), randn(), randn(), randn(), 0.0, -0.01, 0.01, 0.5, 5],
        [init_money*N/M for _ = 1:M],
        [init_money, 0.1*init_money],
        [1.0 for _ = 1:M],
        0.0,
    ) for j = 1:Int(floor(N/2))
]
for _ = 1:(N - Int(floor(N/2)))
    push!(agents, 
        Agent(
            "fundamentals",
            init_money,
            [init_stock_quantity/N for _ = 1:M],
            [init_stock_quantity*init_stock_price/N for _ = 1:M],
            [init_money/0.9],
            [0.9, 5],
            [init_money*N/M for _ = 1:M],
            [init_money, 0.1*init_money],
            [1.0 for _ = 1:M],
            0.0
        )
    )
end

σ_r, σ, μ = 0.2, 2.0, log(100.0)
σ_p = sqrt(σ^2 + σ_r^2)
x = [μ for _ = 1:M]
for t = 1:10^3
    for i = 1:M
        x[i] = exp((log(x[i]) + σ_r*randn())*σ/σ_p + μ*(1 - σ/σ_p))
    end
end
firms = [
    Firm(
        init_stock_price,
        init_stock_quantity,
        [init_stock_price],
        [init_stock_quantity],
        init_stock_price*init_stock_quantity,
        x[i],   #   元init_stock_price*init_stock_quantity
        [],
        [],
    ) for i = 1:M
]



T = 10^4
income_lst = [0.0002*init_money*f(t) for t = 1:T] 
#   income=constantだと、金融市場の規模が継続的に大きく（小さく）ならないためか、
#   適応の結果ポートフォリオ配分目標に占める預金の割合がとても大きくなる。そして売買が減り、値動きが激減する
for t = 1:T
    income = income_lst[t]
    run_one_term(agents, firms, income)
end