package com.polus.fibicomp.award.datesandamounts.dao;

import java.math.BigDecimal;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.datesandamounts.dto.DatesAndAmountsDTO;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardAmountFNADistribution;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardAmountTransactionHistory;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardTransactionStatus;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardAmountTransaction;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardTransactionType;
import com.polus.fibicomp.award.pojo.CostShareType;

@Transactional
@Service
public interface DatesAndAmountDao {

	/**
	 * This method is to get All Award TransactionTypes.
	 * @return List of AwardTransactionType
	 */
	public List<AwardTransactionType> getAllAwardTransactionTypes();

	/**
	 * This method is save Or Update AwardAmountInfo
	 * @param awardAmountInfo - awardAmountInfo
	 * @return Object of AwardAmountInfo
	 */
	public AwardAmountInfo saveOrUpdateAwardAmountInfo(AwardAmountInfo awardAmountInfo);

	/**
	 * This method is save Or Update AwardAmountTransaction
	 * @param awardAmountTransaction - awardAmountTransaction
	 * @return Object of AwardAmountTransaction
	 */
	public AwardAmountTransaction saveOrUpdateAwardAmountTransaction(AwardAmountTransaction awardAmountTransaction);

	/**
	 * This method is get Proposal Number Based On ProposalId
	 * @param fundedProposalId
	 * @return String proposalNumber
	 */
	public String getProposalNumberBasedOnProposalId(Integer proposalId);

	/**
	 * This method is get CostShare Types By AwardId
	 * @param awardId
	 * @return List of AwardCostShare
	 */
	public List<AwardCostShare> getCostShareTypesByAwardId(Integer awardId);

	/**
	 * This method is used to get Child Award Numbers Based On Root Award Number
	 * @param awardNumber - awardNumber
	 * @return List of String
	 */
	public List<String> getAwardNumbersInHierarchy(String awardNumber);

	/**
	 * This method is used to get Child Award Numbers Based On Root Award Number
	 * @param awardNumber - awardNumber
	 * @return List of String
	 */
	public List<String> getChildAwardNumbersBasedOnParentAwardNumber(String awardNumber);

	/**
	 * This method is used to get All Intermediate Child Awards Based On Root Award Number
	 * @param sourceAwardNumber - sourceAwardNumber
	 * @param destinationAwardNumber - destinationAwardNumber
	 * @return object of DatesAndAmountsDTO
	 */
	public DatesAndAmountsDTO getAllIntermediateChildAwards(String sourceAwardNumber, String destinationAwardNumber);

	/**
	 * This method is used to get cost sharing amounts by budgetIncluded flag
	 * 
	 * @param canIncludeInBudget
	 * @return
	 */
	public List<CostShareType> getBudgetIncludedCostshareType(Boolean canIncludeInBudget);

	/**
	 * This method is used delete Award Amount Transaction
	 * 
	 * @param awardAmountTransaction - Object of AwardAmountTransaction
	 */
	public void deleteAwardAmountTransaction(AwardAmountTransaction awardAmountTransaction);

	/**
	 * This method is used to get fetch AwardTransactionStatus based on id
	 * 
	 * @param transactionStatusCode
	 * @return Object of AwardTransactionStatus
	 */
	public AwardTransactionStatus fetchAwardTransactionStatusById(String transactionStatusCode);


	/**
	 * @param awardId
	 * @param awardNumber
	 * @param transactionStatusCode
	 */
	public void updateDatesAndAmounts(Integer awardId, String awardNumber, String transactionStatusCode);

	/**
	 * This method is used to get save AwardAmountTransactionHistory
	 * 
	 * @param awardAmountTransactionHistory
	 * @return Object of AwardAmountTransactionHistory
	 */
	public AwardAmountTransactionHistory saveOrUpdateAwardAmountTransactionHistory(AwardAmountTransactionHistory awardAmountTransactionHistory);

	/**
	 * This method is used to get save fetchLatestTransactionCode
	 * 
	 * @param awardId
	 * @return Transaction Id
	 */
	public BigDecimal fetchLatestTransactionCode(Integer awardId);

	/**
	 * This method is used to getAwardAmountInfoBasedOnParams.
	 * 
	 * @param awardNumber
	 * @param transactionId
	 * @param awardSequenceNumber
	 * @param awardSequenceStatus
	 * @return List of award amount info
	 */
	public List<AwardAmountInfo> getAwardAmountInfoBasedOnParams(String awardNumber, BigDecimal transactionId, Integer awardSequenceNumber, String awardSequenceStatus);

	/**
	 * This method is used to getUsableAwardAmountInfo.
	 * 
	 * @param awardNumber
	 * @return Object of AwardAmountInfo
	 */
	public AwardAmountInfo getUsableAwardAmountInfo(String awardNumber, String transactionStatus);

	/**
	 * This method is used to getLatestAwardAmountInfo.
	 * 
	 * @param awardNumber
	 * @return Object of AwardAmountInfo
	 */
	public AwardAmountInfo getLatestAwardAmountInfo(String awardNumber);

	/**
	 * This method is used to getLatestActiveAwardAmountInfo.
	 * 
	 * @param awardNumber
	 * @return Object of AwardAmountInfo
	 */
	public AwardAmountInfo getLatestActiveAwardAmountInfo(String awardNumber);

	/**
	 * This method is used to getLatestPendingAwardAmountInfo.
	 * 
	 * @param awardNumber
	 * @return Object of AwardAmountInfo
	 */
	public AwardAmountInfo getLatestPendingAwardAmountInfo(String awardNumber);

	/**
	 * This method is used to get deleteTransactionsBasedOnTransactionId
	 * 
	 * @param transactionId
	 */
	public void deleteTransactionsBasedOnTransactionId(BigDecimal transactionId);

	/**
	 * This method is used to get deleteAwardAmountTransaction
	 * 
	 * @param transactionId
	 */
	public void deleteAwardAmountTransaction(BigDecimal transactionId);

	/**
	 * This method is used to get getIsDatesAndAmountsEditable
	 * 
	 * @param awardNumber
	 * @return boolean Value
	 */
	public AwardAmountTransaction getIsDatesAndAmountsEditable(String spiltedAwardNumber);

	/**
	 * @param awardNumber
	 * @return
	 */
	public AwardAmountInfo fetchLastAwardAmountInfo(String awardNumber);

	/**
	 * @param awardAmountInfoId
	 * @return object of AwardAmountInfo
	 */
	public AwardAmountInfo getAwardAmountInfoBasedOnAwardDetail(Integer awardAmountInfoId);

	/**
	 * @param transactionIds
	 * @param awardNumber
	 * @return
	 */
	public List<AwardAmountInfo> getAllUnrelatedTransactions(List<BigDecimal> transactionIds, String awardNumber);

	/**
	 * @param awardAmountFNADistribution
	 * @return AwardAmountFNADistribution
	 */
	AwardAmountFNADistribution saveOrUpdateAwardAmountFNADistribution(AwardAmountFNADistribution awardAmountFNADistribution);

	/**
	 * @param awardNumber
	 * @return
	 */
	Integer getLatestAwardNumber(String awardNumber);

	/**
	 * @param awardId
	 * @return
	 */
	List<AwardAmountFNADistribution> getAwardAmountFNADistributionBasedOnAwardId(Integer awardId);

	/**
	 * @param awardNumber
	 * @param sequenceNumber
	 * @param awardId
	 * @return
	 */
	public List<AwardAmountFNADistribution> getAwardAmountFNADistributionBasedOnParam(String awardNumber,
			Integer sequenceNumber, Integer awardId);

	/**
	 * This method is used to find the award id of last award merged to master with transaction when the award id passing award merged to master 
	 * @param awardId
	 * @param awardNumber
	 * @return
	 */
	Integer getLatestArchiveTransactionAwardBasedOnParam(Integer awardId, String awardNumber);

	/**
	 * @param fnaDistributionId
	 * @return
	 */
	AwardAmountFNADistribution fetchAwardAmountFNADistributionById(Integer fnaDistributionId);

	/**
	 * @param awardAmountFNADistribution
	 */
	void deleteAwardAmountFNADistribution(AwardAmountFNADistribution awardAmountFNADistribution);

}
