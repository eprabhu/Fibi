package com.polus.fibicomp.award.revenue.service;

import java.sql.Timestamp;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Service(value = "awardRevenueService")
public interface AwardRevenueService {

	/**
	 * This method is used to fetch getRevenueDetails.
	 * @param awardNumber
	 * @param accountNumber
	 * @return list of awardRevenueDetails
	 */
	public String loadRevenueDetailsByParams(String awardNumber, String accountNumber);

	/**
	 * This method is used to fetch RevenueTransactions.
	 * @param awardNumber
	 * @param accountNumber
	 * @param internalOrderCode
	 * @param postingDate
	 * @return list of awardRevenueTransactions
	 */
	public String fetchRevenueTransactionsByParams(String awardNumber, String accountNumber, List<String>internalOrderCodes, Timestamp fiPostingStartDate, Timestamp fiPostingEndDate);

}
