package com.polus.fibicomp.awardbudgetapiintegration.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.awardbudgetapiintegration.pojo.BudgetAPIResponse;

@Service
public interface BudgetAPIDao {

	public BudgetAPIResponse saveBudgetAPIResponse(BudgetAPIResponse budgetAPIResponse);
	
	public List<BudgetAPIResponse> fetchBudgetAPIResponse(String projectNumber, Integer year);

	public String getBudgetIntegrationConfigurationValue(String configurationKey);
}
