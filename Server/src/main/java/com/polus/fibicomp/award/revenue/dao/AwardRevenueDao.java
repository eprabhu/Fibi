package com.polus.fibicomp.award.revenue.dao;

import java.sql.Timestamp;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.revenue.pojo.AwardRevenueDetails;
import com.polus.fibicomp.award.revenue.pojo.AwardRevenueTransactions;

@Transactional
@Service(value = "awardRevenueDao")
public interface AwardRevenueDao {

	/**
	 * This method is used to fetch getRevenueDetails.
	 * @param awardNumber
	 * @param accountNumber
	 * @return list of awardRevenueDetails
	 */
	public List<AwardRevenueDetails> getRevenueDetailsByParams(String awardNumber, String accountNumber);

	/**
	 * This method is used to fetch RevenueTransactions.
	 * @param awardNumber
	 * @param accountNumber
	 * @param internalOrderCode
	 * @param postingDate
	 * @return list of awardRevenueTransactions
	 */
	public List<AwardRevenueTransactions> fetchRevenueTransactionsByParams(String awardNumber, String accountNumber,
			List<String> internalOrderCodes, Timestamp fiPostingStartDate, Timestamp fiPostingEndDate);

}
