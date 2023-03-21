package com.polus.fibicomp.award.expense.service;

import java.util.List;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;
import com.polus.fibicomp.award.expense.vo.AwardExpenseDetailVO;
import com.polus.fibicomp.award.expense.vo.AwardExpenseTransactionVO;
import com.polus.fibicomp.award.expense.vo.AwardHoursLoggedVO;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "awardExpenseService")
public interface AwardExpenseService {

	public String fetchExpenditureTransactions(AwardExpenseTransactionVO vo);

	/**
	 * This method is used to fetch expenditure transaction details.
	 * @param accountNumber-     Object of award expense transaction.
	 * @param internalOrderCode- Object of award expense transaction.
	 * @return An object of award expense transaction.
	 */
	public String fetchPersonLogDetails(AwardHoursLoggedVO vo);

	/**
	 * This method is used to fetch person log details.
	 * @param awardNumber-       Object of AwardPersonLogged.
	 * @param accountNumber-     Object of AwardPersonLogged.
	 * @param type - expense or purchase
	 * @return An object of AwardPersonLogged.
	 */
	public String loadExpenseDetailsByAwardId(String awardNumber, String accountNumber, String type);

	/**
	 * This method is used to fetch expense details
	 * 
	 * @param awardNumber-   Object of award expense.
	 * @param accountNumber- Object of award expense
	 * @return An object of award expense.
	 */
	public String updateCommittedAmount(AwardExpenseDetailVO vo);

	/**
	 * This method is used to fetch expenditure transaction details.
	 * 
	 * @param accountNumber-     Object of award expense transaction.
	 * @param internalOrderCode- Object of award expense transaction.
	 * @return An object of award expense transaction.
	 */
	public String fetchExpenseTransactionsDetails(AwardExpenseTransactionVO vo);

	/**
	 * This method is used to get XSSFWorkbook based on index tab clicked in dash board.
	 * @param vo - object of AwardExpenseDetailsExtVO.
	 * @param XSSFWorkbook for excel sheet preparation.
	 * @return XSSFWorkbookthat contains excel sheet with data.
	 * @throws Exception
	 */
	public XSSFWorkbook getXSSFWorkbookForExpenseTracking(CommonVO vo) throws Exception;

	/**
	 * This method is used to AwardExpenseDetailsExt
	 * @param vo-       Object of AwardExpenseDetailsVO.
	 * @return An JSON of AwardExpenseDetailsExt.
	 */
	public String fetchAwardExpenseDetailsExtByParams(AwardExpenseDetailVO vo);

	/**
	 * This method is used to AwardExpenseDetailsVO
	 * @param awardExpenseDetailsId- Id of AwardExpenseDetailsExt.
	 * @return JSON list of AwardExpenseDetailsExt.
	 */
	public String deleteAwardExpenseDetailsExtById(AwardExpenseDetailVO vo);

	/**
	 * This method is used to fetch Award Expense Person Details
	 * 
	 * @param budgetDetailId- Object budgetDetailId.
	 * @return An String response.
	 */
	public String loadAwardExpensePersonDetails(Integer budgetDetailId, String awardNumber, String accountNumber);

	/**
	 * This method is used to load ExpenseDetails As Group By awardNumber
	 * 
	 * @param awardNumber- awardNumber.
	 * @return An String response.
	 */
	public String loadExpenseDetailsBasedOnCostElement(String awardNumber, String accountNumber, String type);

	/**
	 * This method is used to list award expense transaction details.
	 * @param accountNumber
	 * @param internalOrderCode
	 * @param actualOrCommittedFlag
	 * @param awardNumber
	 * @param fiPostingDate
	 * @return
	 */
	public List<AwardExpenseTransaction> getExpenseTransactions(String accountNumber, String internalOrderCode, String actualOrCommittedFlag, String awardNumber, String fmPostingStartDate, String fmPostingEndDate, String isShowAllTransactions);

	public List<AwardExpenseDetailVO> prepareAwardExpenseDetails(String awardNumber, String accountNumber, String type);
}
