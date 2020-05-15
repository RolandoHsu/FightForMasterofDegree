# 新上市櫃公司失敗風險預測

### 論文方向簡介 ###
新股上市上櫃稱為IPO，在過去的研究中，不論國內外，有相當多的文獻發現IPO公司會有上市初期異常正報酬，到了長期，則呈現反轉而報酬衰減的現象。然而，卻較少有文獻提及IPO公司的失敗、下市預測。一間公司的下市，對於投資人來說，肯定是最為慘烈的情況，不僅使投資金血本無歸，更可能極大程度的影響到未來的育兒、退休等重大規劃。因此，找出可能影響IPO下市的因素即成為一個相當重要的議題。

興櫃市場是公司在進行上市櫃前，需要先在其上進行交易的市場，有點像是一個新上市櫃公司的試煉場，給予投資人投資資訊，若在興櫃市場中公司並沒有較好的表現或出現壞的紀錄（例如：重大財務危機。），則容易帶給投資人不好的印象導致交易量大減、股價暴跌甚至不容易於上市櫃中生存。而台灣，身為全世界唯一一個強制要在上市櫃前先興櫃六個月的國家，相比於其他國家，有更多的公司交易、體質資料可供研究，因而成為研究IPO下市與否的重要場所。因此我選定台灣作為研究國家，探討2002~2014年間，IPO公司的下市機率與影響因素，並透過不同的Random Forest、XGB等模型，建構預測模型，並比較不同模型所做出的預測差異。
  
### 流程說明 ###

1. FindFirmList.Rmd

	透過 FindFirmList.Rmd 尋找符合條件的公司（EX: 挑選出2002~2014年之IPO公司，並排除金融股或興櫃期間不滿半年之公司），最終確認樣本數為604家公司。
	透過圖表等方式做EDA，探討台灣IPO公司之特性。

2. DealWithIPOVariables.Rmd

	透過 DealWithIPOVariables.Rmd 整理所有IPO特性相關變數。

3. FSVariables.Rmd

	透過 FSVariables.Rmd 整理IPO公司興櫃期間之財報變數，並將其與相對應的市場及產業平均比較。

4. ROTCTradingVariable.Rmd

	透過 ROTCTradingVariable.Rmd 整理所有IPO公司在興櫃期間之交易變數，例如0交易量的天數等。

5. BuildLogisticRegressionModel_2.Rmd

	透過BuildLogisticRegressionModel_2.Rmd 將所有變數整合後丟入 Logistic Regression 中，透過多次CV之結果發現Logistic Regression 相對於其他兩個模型來說會有較好的表現(AUC 平均為 78.09 %)。
	
6. RandomForest.Rmd
	
	透過 RandomForest.Rmd 將所有變數整合後丟入 Random Forest 中，透過多次CV後發現 AUC 平均為 75.39 %。
	
7. XGBoost.Rmd
	
	透過 XGBoost.Rmd 將所有變數整合後丟入 Extreme Gradient Boosting 中，透過多次CV後發現 AUC 平均為 74.42 %。

8. Variables_Summary.Rmd
	
	將所有變數透過IPO是否會失敗做區分，計算平均後，比較IPO失敗與否之平均差異，藉以找出不同群組(IPO失敗與否)之變數平均差異。
	
	
### 論文結論 ###
	
1. 興櫃市場變數的確會是預測IPO是否會失敗之重要變數，觀察在不同的三個模型之重要性排序後，發現 營業利益成長率、存貨周轉率、研究發展費用率、營業成本、營業利益率、應收帳款週轉率、是否有負的NetIncome 為預測IPO是否會失敗的重要變數。

2. IPO特性變數中以 IPO前公司年齡、平均中籤率、會計是否四大會計師事務所 三項變數最為重要(統計亦為顯著)。

3. 比較多次交叉驗證後發現，Logistic Regression 能較好的預測IPO是否會失敗(平均AUC值 : 78.09 %)，Random Forest 第二 (平均AUC值 : 75.39%)。

4. 觀察樣本外預測之結果發現，Random Forest 能較好的預測出IPO失敗的公司。(AUC : 88.93 且test data之九個失敗公司中成功預測8個。)

5. 加入興櫃市場之變數後，的確能提升模型預測能力。(Logistic Regression、Random Forest、XGB 之多次CV平均AUC均有所提升，且Logistic Regression 之R Squared 亦有所上升。)
	
	
	
