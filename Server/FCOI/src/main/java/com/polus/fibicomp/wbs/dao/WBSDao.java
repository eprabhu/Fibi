package com.polus.fibicomp.wbs.dao;

import org.springframework.stereotype.Service;

@Service
public interface WBSDao {

	/**
	 * This method is used to generate the wbs number based on awards details
	 * @param awardId
	 * @param isGenerateAccountNumber
	 * @param budgetDetailId
	 * @param budgetCategoryCode
	 * @return true is the number is generated successfully else error message
	 */
	public String generateWBSNumber(Integer awardId, String isGenerateAccountNumber, Integer budgetDetailId, String budgetCategoryCode);

	/**
	 * This method is used to generate the wbs number based on awards details
	 * @param awardId
	 * @param budgetHeaderId
	 * @param flag
	 * @param costElement
	 * @param budgetPeriodId
	 * @return true is the number is generated successfully else error message
	 */
	public String generateIOCode(Integer awardId,  Integer budgetHeaderId,String flag,String costElement,Integer budgetPeriodId);
}
