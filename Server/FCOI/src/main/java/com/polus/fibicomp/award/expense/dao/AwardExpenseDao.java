package com.polus.fibicomp.award.expense.dao;

import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetail;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetailsExt;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseHeader;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;
import com.polus.fibicomp.award.expense.pojo.AwardHoursLogged;
import com.polus.fibicomp.award.expense.vo.AwardExpenseTransactionVO;

/**
 * @author sajith.m
 *
 */
@Transactional
@Service(value = "awardExpenseDao")
public interface AwardExpenseDao {

	/**
	 * This method is used to fetch award expense transaction details
	 * 
	 * @param awardNumber-       Object of award expense transaction.
	 * @param accountNumber-     Object of award expense transaction.
	 * @param internalOrderCode- Object of award expense transaction.
	 * @return An object of award expense transaction.
	 */
	public List<AwardExpenseTransactionVO> fetchAwardExpenseTransactionsByParams(String awardNumber,String accountNumber, String internalOrderCode);

	/**
	 * This method is used to fetch person log details
	 * 
	 * @param awardNumber-       Object of AwardPersonLogged.
	 * @param accountNumber-     Object of AwardPersonLogged.
	 * @param internalOrderCode- Object AwardPersonLogged.
	 * @return An object of AwardPersonLogged.
	 */
	public List<AwardHoursLogged> fetchPersonLogDetailsByParams(String awardNumber, String accountNumber, String internalOrderCode);

	/**
	 * This method is used to fetch expense details
	 * 
	 * @param awardNumber-       Object of award expense.
	 * @param accountNumber-     Object of award expense.
	 * @param type 
	 * @return An object of ResultSet.
	 */
	public ResultSet loadExpenseDetailsByAwardId(String awardNumber, String accountNumber, String type);

	/**
	 * This method is used to save or update commitment amount.
	 * 
	 * @param AwardExpenseDetailsExt - Object of a AwardExpenseDetailsExt.
	 * @return An object of AwardExpenseDetailsExt.
	 */
	public AwardExpenseDetailsExt updateCommittedAmount(AwardExpenseDetailsExt awardExpenseDetailsExt);

	/**
	 * This method is used to AwardExpenseDetailsExt
	 * 
	 * @param awardNumber-       Object of AwardPersonLogged.
	 * @param accountNumber-     Object of AwardPersonLogged.
	 * @param internalOrderCode- Object AwardPersonLogged.
	 * @return An list of AwardExpenseDetailsExt.
	 */
	public List<AwardExpenseDetailsExt> fetchAwardExpenseDetailsExtByParams(String awardNumber, String accountNumber, String internalOrderCode);

	/**
	 * This method is used to fetch award expense transaction details
	 * @param awardNumber-       Object of award expense transaction.
	 * @param accountNumber-     Object of award expense transaction.
	 * @param internalOrderCode- Object of award expense transaction.
	 * @param actualOrCommittedFlag- Object of award expense transaction.
	 * @return An object of award expense transaction.
	 */
	public List<AwardExpenseTransaction> fetchExpenseTransactionsDetailsByParams(String awardNumber, String accountNumber, String internalOrderCode, String actualOrCommittedFlag);

	/**
	 * This method is used to get AwardExpenseDetail
	 * @param awardNumber
	 * @param accountNumber
	 * @param internalOrderCode
	 * @return An object of AwardExpenseDetail.
	 */
	public AwardExpenseDetail fetchAwardExpenseDetailByParams(String awardNumber, String accountNumber, String internalOrderCode);

	/**
	 * This method is used to delete AwardExpenseDetailsExt
	 * @param awardExpenseDetailsExt - Object AwardExpenseDetailsExt.
	 * @return void.
	 */
	public void deleteAwardExpenseDetailsExtById(AwardExpenseDetailsExt awardExpenseDetailsExt);

	/**
	 * This method is used to get AwardExpenseDetailsExt
	 * @param awardExpenseDetailsId - Id of AwardExpenseDetailsExt.
	 * @return Object AwardExpenseDetailsExt.
	 */
	public AwardExpenseDetailsExt getAwardExpenseDetailsExtById(Integer awardExpenseDetailsId);

	/**
	 * This method is used to get AwardExpenseHeader
	 * @param awardNumber - AwardNumber.
	 * @param accountNumber - AccountNumber.
	 * @return Object of AwardExpenseHeader.
	 */
	public AwardExpenseHeader getAwardExpenseHeader(String awardNumber, String accountNumber);

	/**
	 * This method is used to fetch Award Expense Person Details
	 * 
	 * @param budgetDetailId- Object of award expense.
	 * @return An object of ResultSet.
	 */
	public ResultSet loadAwardExpensePersonDetails(Integer budgetDetailId, String awardNumber, String accountNumber);

	/**
	 * This method is used to load Expense Details As Group
	 * @param accountNumber 
	 * 
	 * @param budgetDetailId- Object of award expense.
	 * @return An object of ResultSet.
	 */
	public ResultSet loadExpenseDetailsAsGroup(String awardNumber, String accountNumber, String type);

	/**
	 * This method is used to fetch ExpenseTransactionsDetails
	 * 
	 * @param accountNumber- accountNumber.
	 * @param internalOrderCode- internalOrderCode.
	 * @param actualOrCommittedFlag - actualOrCommittedFlag
	 * @param fiPostingDate - fiPostingDate
	 * @param isShowAllTransactions
	 * @return An object of ResultSet.
	 */
	public ResultSet fetchExpenseTransactionsDetails(String accountNumber, String internalOrderCode, String actualOrCommittedFlag, String awardNumber, String fmPostingStartDate, String fmPostingEndDate, String isShowAllTransactions);

	/**
	 * This method is used to fetch person log details
	 * 
	 * @param awardNumber-       awardNumber
	 * @param accountNumber-     accountNumber.
	 * @param internalOrderCode- ioCodes.
	 * @return A List of AwardExpenseDetailsExt.
	 */
	public List<AwardExpenseDetailsExt> fetchAwardExpenseDetailsExtByParams(String awardNumber, String accountNumber, List<String> ioCodes);

	/** This method is used to fetch the expense details from expese details extension table
	 * @param awardNumber
	 * @param accountNumber
	 * @param internalOrderCode
	 * @return Object of 
	 */
	public AwardExpenseDetailsExt getAwardExpenseExtDetailsByParams(String awardNumber, String accountNumber, String internalOrderCode);

	/**
	 * @param awardNumber
	 * @param accountNumber
	 * @param startDate
	 * @param endDate
	 * @param actualOrCommittedFlag
	 * @param awardExpenseTransactionId 
	 * @return List of AwardExpenseTransaction
	 */
	public List<AwardExpenseTransaction> getAwardExpenseTransactionBasedOnClaims(String awardNumber, String accountNumber, Timestamp startDate, Timestamp endDate, String actualOrCommittedFlag, List<Integer> awardExpenseTransactionId);

	/**
	 * @param awardNumber
	 * @param accountNumber
	 * @param startDate
	 * @param endDate
	 * @param actualOrCommittedFlag
	 * @param object
	 * @return List of AwardExpenseTransaction
	 */
	public List<AwardExpenseTransaction> getAwardExpenseCommittedTransactionForClaim(String awardNumber,
			String accountNumber, Timestamp startDate, Timestamp endDate);

}
