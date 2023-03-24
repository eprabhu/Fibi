package com.polus.fibicomp.award.datesandamounts.service;

import java.math.BigDecimal;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.datesandamounts.dto.AwardAmountFNADistributionDTO;
import com.polus.fibicomp.award.datesandamounts.pojo.AwardAmountFNADistribution;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.vo.AwardDatesandAmountVO;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.ip.pojo.InstituteProposal;

@Transactional
@Service
public interface DatesAndAmountService {

	/**
	 * This method is to get AwardAmountInfo.
	 * @param awardDatesandAmountVO - awardDatesandAmountVO
	 * @return String Value
	 */
	public String getAwardDatesAndAmount(AwardDatesandAmountVO awardDatesandAmountVO) throws Exception;

	/**
	 * This method is to save TransactionDetails.
	 * @param awardDatesandAmountVO - awardDatesandAmountVO
	 * @return String of awardDatesandAmountVO
	 */
	public String saveTransactionDetails(AwardDatesandAmountVO awardDatesandAmountVO) throws Exception;

	/**
	 * This method is to get getAwardCostShare.
	 * @param awardVO - awardVO
	 * @return String of awardVO
	 */
	public String getAwardCostShare(AwardVO awardVO);

	/**
	 * This method is to getAwardFunds.
	 * @param awardVO - awardVO
	 * @return String of awardVO
	 */
	public String getAwardFunds(AwardVO awardVO);

	/**
	 * This method is to createAwardAmountInfo.
	 * @param award - award
	 * @param instituteProposal - instituteProposal
	 * @param budgetHeader 
	 * @param updateUser - updateUser
	 * @return Object of AwardAmountInfo
	 */
	public AwardAmountInfo createAwardAmountInfo(Award award, InstituteProposal instituteProposal, BudgetHeader budgetHeader, String updateUser);

	/**
	 * This method is to save Total Project Cost In Foreign Currency.
	 * @param vo - AwardDatesandAmountVO
	 * @return String of AwardDatesandAmountVO
	 */
	public String saveTotalProjectCostInForeignCurrency(AwardDatesandAmountVO vo);

	/**
	 * This method is to update Dates And Amounts Status and add entry to History table.
	 * @param awardId - awardId
	 * @param awardNumber - awardNumber
	 * @param sequenceNumber - sequenceNumber
	 * @param transactionStatus - transactionStatus
	 */
	public void updateDatesAndAmounts(Integer awardId, String awardNumber, Integer sequenceNumber, String transactionStatus);

	/**
	 * This method is to prepareAwardAmountInfoForAward.
	 * @param awardId - awardId
	 * @param awardNumber - awardNumber
	 * @param sequenceNumber - sequenceNumber
	 */
	public List<AwardAmountInfo> prepareAwardAmountInfoForAward(Integer awardId, String awardNumber, Integer sequenceNumber);

	/**
	 * @param transactionId
	 * @param isUpdate
	 * @return
	 */
	public String deleteTransactionDetails(BigDecimal transactionId);

	/**
	 * @param awardId
	 * @param awardNumber
	 * @param awardSequenceNumber
	 * @param awardSequenceStatus
	 * @return
	 */
	List<AwardAmountInfo> getAwardAmountInfoBasedOnParams(Integer awardId, String awardNumber, Integer awardSequenceNumber, String awardSequenceStatus);

	/**
	 * @param awardAmountFNADistributionDTO
	 * @return
	 */
	String saveOrUpdateAnticipatedDistribution(List<AwardAmountFNADistribution> awardAmountFNADistributionDTO);

	/**
	 * @param awardNumber
	 * @param sequenceNumber
	 * @return
	 */
	String loadAnticipatedDistribution(AwardAmountFNADistributionDTO awardAmountFNADistributionDTO);

	/**
	 * @param fnaDistributionId
	 * @return
	 */
	String deleteAnticipatedDistribution(Integer fnaDistributionId);
}
