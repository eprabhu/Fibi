package com.polus.fibicomp.award.expense.service;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.polus.fibicomp.common.service.CommonService;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.expense.comparator.AwardExpenseDetailsComparatorBySortOrder;
import com.polus.fibicomp.award.expense.dao.AwardExpenseDao;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseDetailsExt;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseHeader;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;
import com.polus.fibicomp.award.expense.pojo.AwardHoursLogged;
import com.polus.fibicomp.award.expense.vo.AwardExpenseDetailVO;
import com.polus.fibicomp.award.expense.vo.AwardExpenseTransactionVO;
import com.polus.fibicomp.award.expense.vo.AwardHoursLoggedVO;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetPersonalDetail;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.rolodex.dao.RolodexDao;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "awardExpenseService")
public class AwardExpenseServiceImpl implements AwardExpenseService {

	protected static Logger logger = LogManager.getLogger(AwardExpenseServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "awardExpenseDao")
	private AwardExpenseDao awardExpenseDao;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	public PersonDao personDao;

	@Autowired
	public RolodexDao rolodexDao;

	@Autowired
	public AwardBudgetDao awardBudgetDao;

	@Autowired
	public PrintService printService;

	@Autowired
	CommonService commonService;

	DecimalFormat numberFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITHOUT_DECIMAL);
	DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);

	@Override
	public String loadExpenseDetailsByAwardId(String awardNumber, String accountNumber, String type) {
		AwardExpenseHeader awardExpenseHeader = awardExpenseDao.getAwardExpenseHeader(awardNumber, accountNumber);
		Boolean manpowerEnabled = commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED);
		Boolean budgetAssociatedWithManpower = commonDao.getParameterValueAsBoolean(Constants.IS_BUDGET_ASSOCIATED_WITH_MANPOWER);
		if (awardExpenseHeader != null) {
			awardExpenseHeader.setAwardExpenseDetailVOs(prepareAwardExpenseDetails(awardNumber, accountNumber, type));
			awardExpenseHeader.setManpowerEnabled(manpowerEnabled);
			awardExpenseHeader.setBudgetAssociatedWithManpower(budgetAssociatedWithManpower);
			return commonDao.convertObjectToJSON(awardExpenseHeader);
		} else {
			AwardExpenseHeader awardExpenseHeaderData = new  AwardExpenseHeader();
			awardExpenseHeaderData.setAwardExpenseDetailVOs(prepareAwardExpenseDetails(awardNumber, accountNumber, type));
			awardExpenseHeaderData.setManpowerEnabled(manpowerEnabled);
			awardExpenseHeaderData.setBudgetAssociatedWithManpower(budgetAssociatedWithManpower);
			return commonDao.convertObjectToJSON(awardExpenseHeaderData);
		}
	}

	@Override
	public String fetchExpenditureTransactions(AwardExpenseTransactionVO vo) {
		return commonDao.convertObjectToJSON(awardExpenseDao.fetchAwardExpenseTransactionsByParams(vo.getAwardNumber(), vo.getAccountNumber(), vo.getInternalOrderCode()));
	}

	@Override
	public String fetchPersonLogDetails(AwardHoursLoggedVO vo) {
		List<AwardHoursLogged> awardHoursLoggeds = awardExpenseDao.fetchPersonLogDetailsByParams(vo.getAwardNumber(), vo.getAccountNumber(), vo.getInternalOrderCode());
		BigDecimal totalSubmittedhours = BigDecimal.ZERO;
		BigDecimal totalPayrollhours = BigDecimal.ZERO;
		if (awardHoursLoggeds != null && !awardHoursLoggeds.isEmpty()) {
			for (AwardHoursLogged awardHoursLogged : awardHoursLoggeds) {
				if (awardHoursLogged.getSubmittedhours() != null) {
					totalSubmittedhours = totalSubmittedhours.add(awardHoursLogged.getSubmittedhours());
				}
				if (awardHoursLogged.getPayrollhours() != null) {
					totalPayrollhours = totalPayrollhours.add(awardHoursLogged.getPayrollhours());
				}
			}
		}
		vo.setAwardHoursLogged(awardHoursLoggeds);
		vo.setTotalSubmittedhours(totalSubmittedhours);
		vo.setTotalPayrollhours(totalPayrollhours);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String updateCommittedAmount(AwardExpenseDetailVO vo) {
		String awardNumber = vo.getAwardNumber();
		String accountNumber = vo.getAccountNumber();
		String internalOrderCode = vo.getInternalOrderCode();
		try {
			AwardExpenseDetailsExt expenseDetailsExt = new AwardExpenseDetailsExt();
			expenseDetailsExt.setAwardNumber(awardNumber);
			expenseDetailsExt.setAccountNumber(accountNumber);
			expenseDetailsExt.setInternalOrderCode(internalOrderCode);
			expenseDetailsExt.setIsFromSap("N");
			expenseDetailsExt.setUpdateUser(vo.getUpdateUser());
			expenseDetailsExt.setCommittedAmount(vo.getBalance());
			expenseDetailsExt.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			expenseDetailsExt.setDescription(vo.getDescription());
			expenseDetailsExt = awardExpenseDao.updateCommittedAmount(expenseDetailsExt);
			vo.setAwardExpenseDetailsExts(awardExpenseDao.fetchAwardExpenseDetailsExtByParams(awardNumber, accountNumber, internalOrderCode));
			vo.setBalance(BigDecimal.ZERO);
		} catch (Exception e) {
			logger.info("error in saveOrUpdateCommittedAmount");
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchExpenseTransactionsDetails(AwardExpenseTransactionVO vo) {
		String actualOrCommittedFlag = vo.getActualOrCommittedFlag();
		String awardNumber = vo.getAwardNumber();
		String accountNumber = vo.getAccountNumber();
		String internalOrderCodes = (vo.getInternalOrderCodes() != null && !vo.getInternalOrderCodes().isEmpty()) ? String.join(",", vo.getInternalOrderCodes()): null;
		String fmPostingStartDate = vo.getFmPostingStartDate();
		String fmPostingEndDate = vo.getFmPostingEndDate();
		String isShowAllTransactions = vo.getIsShowAllTransactions();
		vo.setExpenseTransactions(getExpenseTransactions(accountNumber, internalOrderCodes, actualOrCommittedFlag, awardNumber, fmPostingStartDate, fmPostingEndDate, isShowAllTransactions));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public List<AwardExpenseTransaction> getExpenseTransactions(String accountNumber, String internalOrderCode, String actualOrCommittedFlag, String awardNumber, String fmPostingStartDate, String fmPostingEndDate, String isShowAllTransactions) {
		List<AwardExpenseTransaction> awardExpenseTransactions = new ArrayList<>();
		ResultSet resultSet = awardExpenseDao.fetchExpenseTransactionsDetails(accountNumber, internalOrderCode, actualOrCommittedFlag, awardNumber, fmPostingStartDate, fmPostingEndDate, isShowAllTransactions);
		try {
			while (resultSet.next()) {
				AwardExpenseTransaction awardExpenseTransaction = new  AwardExpenseTransaction();
				if (resultSet.getString("AWARD_EXPENSE_TRANS_ID") != null) {
					awardExpenseTransaction.setAwardExpenseTransactionId(Integer.parseInt(resultSet.getString("AWARD_EXPENSE_TRANS_ID")));
				}
				awardExpenseTransaction.setRemarks(resultSet.getString("REMARKS"));
				awardExpenseTransaction.setVendorCode(resultSet.getString("VENDOR_CODE"));
				awardExpenseTransaction.setVendorName(resultSet.getString("VENDOR_NAME"));
				awardExpenseTransaction.setFiGlAccount(resultSet.getString("FI_GL_ACCOUNT"));
				awardExpenseTransaction.setBankClearingDate(resultSet.getTimestamp("BANK_CLEARING_DATE"));
				awardExpenseTransaction.setFmPostingDate(resultSet.getTimestamp("FM_POSTING_DATE"));
				awardExpenseTransaction.setDocumentDate(resultSet.getTimestamp("DOCUMENT_DATE"));
				awardExpenseTransaction.setPrDate(resultSet.getTimestamp("PR_DATE"));
				awardExpenseTransaction.setPoDate(resultSet.getTimestamp("PO_DATE"));
				awardExpenseTransaction.setGrDate(resultSet.getTimestamp("GR_DATE"));
				awardExpenseTransaction.setInvoiceDate(resultSet.getTimestamp("INVOICE_DATE"));
				awardExpenseTransaction.setRequesterName(resultSet.getString("REQUESTER_NAME"));
				awardExpenseTransaction.setAwardNumber(resultSet.getString("AWARD_NUMBER"));
				awardExpenseTransaction.setAccountNumber(resultSet.getString("ACCOUNT_NUMBER"));
				awardExpenseTransaction.setInternalOrderCode(resultSet.getString("INTERNAL_ORDER_CODE"));
				awardExpenseTransaction.setUpdateUser(resultSet.getString("UPDATE_USER"));
				awardExpenseTransaction.setPoPrFlag(resultSet.getString("PO_PR_FLAG"));
				awardExpenseTransaction.setFiGlDescription(resultSet.getString("FI_GL_DESCRIPTION"));
				awardExpenseTransaction.setUpdateTimestamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				awardExpenseTransaction.setReferenceDocNumber(resultSet.getString("REFERENCE_DOC_NUMBER"));
				awardExpenseTransaction.setTransactionReferenceNumber(resultSet.getString("TRANSACTION_REFERENCE_NUMBER"));
				awardExpenseTransaction.setDocumentNumber(resultSet.getString("DOCUMENT_NUMBER"));
				if (resultSet.getBigDecimal("AMOUNT_IN_FMA_CURRENCY") != null) {
					awardExpenseTransaction.setAmountInFmacurrency(resultSet.getBigDecimal("AMOUNT_IN_FMA_CURRENCY"));
				} else {
					awardExpenseTransaction.setAmountInFmacurrency(BigDecimal.ZERO);
				}
				awardExpenseTransactions.add(awardExpenseTransaction);
			}
		} catch (Exception e) {
			logger.info(e.getMessage());
		}
		return awardExpenseTransactions;
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookForExpenseTracking(CommonVO vo) throws Exception {
		XSSFWorkbook workbook = new XSSFWorkbook();
		List<AwardExpenseDetailVO> expenseDetails = prepareAwardExpenseDetails(vo.getAwardNumber(), vo.getAccountNumber(), vo.getType());
		XSSFSheet sheet = workbook.createSheet("Expense Details");
		Object[] tableHeadingRow = { "BudgetCategory", "Budget Line Item", "WBS Number", "Quantity", "Original Approved Budget ("+Constants.DOLLAR_SYMBOL+")", 
				"Latest Approved Budget ("+Constants.DOLLAR_SYMBOL+")", "Expenditure to Date ("+Constants.DOLLAR_SYMBOL+")", "Balance ("+Constants.DOLLAR_SYMBOL+")", "Utilization Rate", "Committed Budget ("+Constants.DOLLAR_SYMBOL+")", "Balance Less Committed Budget ("+Constants.DOLLAR_SYMBOL+")"};
		commonService.addDetailsInHeader(workbook,sheet);
		prepareExcelSheetForExpenseTracking(expenseDetails, sheet, tableHeadingRow, workbook, vo);
		return workbook;
	}

	@SuppressWarnings("deprecation")
	private void prepareExcelSheetForExpenseTracking(List<AwardExpenseDetailVO> expenseDetails, XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, CommonVO vo) {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		XSSFCellStyle currencyStyle = (XSSFCellStyle) workbook.createCellStyle();
		currencyStyle.setAlignment(HorizontalAlignment.RIGHT);
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 1));
		int rowNumber = 0;
		int cellNumber = 0;
		rowNumber = printService.prepareAwardGeneralInformationForExcel(sheet, rowNumber, vo.getAwardId(), workbook, null, Boolean.FALSE);
		String heading = "";
		if (vo.getType().equals("E")) {
			heading = "Expense Details";
		} else {
			heading = "Purchase Details";
		}
		++rowNumber;
		workbook = printService.prepareExcelSheetHeading(sheet, heading, workbook, tableHeadingRow.length, rowNumber);
		++rowNumber;
		workbook = printService.prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		BigDecimal totalQuantity = BigDecimal.ZERO;
		BigDecimal totalOriginalApprovedBudget = BigDecimal.ZERO;
		BigDecimal totalLatestApprovedBudget = BigDecimal.ZERO;
		BigDecimal totalExpenditureToDate = BigDecimal.ZERO;
		BigDecimal totalBalance = BigDecimal.ZERO;
		BigDecimal totalUtilizationRate = BigDecimal.ZERO;
		BigDecimal totalCommittedAmount = BigDecimal.ZERO;
		BigDecimal totalBalanceLessCommittedAmount = BigDecimal.ZERO;
		BigDecimal totalUtilizationAmount = BigDecimal.ZERO;
		String percentage = "%";
		for (AwardExpenseDetailVO awardExpenseDetails : expenseDetails) {
			Row row = sheet.createRow(rowNumber++);
			cellNumber = 0;
			Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardExpenseDetails.getBudgetCategory() != null)
				cell1.setCellValue(awardExpenseDetails.getBudgetCategory());
			else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardExpenseDetails.getLineItem() != null)
				cell2.setCellValue(awardExpenseDetails.getLineItem());
			else
				cell2.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardExpenseDetails.getInternalOrderCode() != null)
				cell3.setCellValue(awardExpenseDetails.getInternalOrderCode());
			else
				cell3.setCellValue(" ");
			cellNumber++;

			Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardExpenseDetails.getQuantity() != null) {
				totalQuantity = totalQuantity.add(awardExpenseDetails.getQuantity());
				cell4.setCellValue(numberFormat.format(awardExpenseDetails.getQuantity()));
			}
			else
				cell4.setCellValue(" ");
			cellNumber++;

			Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
			cell5.setCellStyle(currencyStyle);
			if (awardExpenseDetails.getOriginalApprovedBudget() != null) {
				totalOriginalApprovedBudget = totalOriginalApprovedBudget.add(awardExpenseDetails.getOriginalApprovedBudget());
				cell5.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(awardExpenseDetails.getOriginalApprovedBudget()));
			}
			else
				cell5.setCellValue(" ");
			cellNumber++;

			Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
			cell6.setCellStyle(currencyStyle);
			if (awardExpenseDetails.getLatestApprovedBudget() != null) {
				totalLatestApprovedBudget = totalLatestApprovedBudget.add(awardExpenseDetails.getLatestApprovedBudget());
				cell6.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(awardExpenseDetails.getLatestApprovedBudget()));
			}
			else
				cell6.setCellValue(" ");
			cellNumber++;

			Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
			cell7.setCellStyle(currencyStyle);
			if (awardExpenseDetails.getExpenditureToDate() != null) {
				totalExpenditureToDate = totalExpenditureToDate.add(awardExpenseDetails.getExpenditureToDate());
				cell7.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(awardExpenseDetails.getExpenditureToDate()));
			}	
			else
				cell7.setCellValue(" ");
			cellNumber++;

			Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
			cell8.setCellStyle(currencyStyle);
			if (awardExpenseDetails.getBalance() != null) {
				totalBalance = totalBalance.add(awardExpenseDetails.getBalance());
				cell8.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(awardExpenseDetails.getBalance()));
			}
			else
				cell8.setCellValue(" ");
			cellNumber++;

			Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
			cell9.setCellStyle(currencyStyle);
			if (awardExpenseDetails.getUtilizationRate() != null) {
				totalUtilizationRate = totalUtilizationRate.add(awardExpenseDetails.getUtilizationRate());
				cell9.setCellValue(decimalFormat.format(awardExpenseDetails.getUtilizationRate()) + percentage);
			}
			else
				cell9.setCellValue("0.0" + percentage);
			cellNumber++;

			Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
			cell10.setCellStyle(currencyStyle);
			BigDecimal committedAmount = BigDecimal.ZERO;
			committedAmount = awardExpenseDetails.getCommittedAmount();
			if (committedAmount != null) {
				totalCommittedAmount = totalCommittedAmount.add(committedAmount);
				cell10.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(committedAmount));
			}	
			else
				cell10.setCellValue(" ");
			cellNumber++;

			Cell cell11 = assignCell(cellNumber, tableBodyStyle, row);
			cell11.setCellStyle(currencyStyle);
			BigDecimal balanceLessCommittedAmount = BigDecimal.ZERO;
			balanceLessCommittedAmount = awardExpenseDetails.getBalanceCommittedBudget();
			if (balanceLessCommittedAmount != null) {
				totalBalanceLessCommittedAmount = totalBalanceLessCommittedAmount.add(balanceLessCommittedAmount);
				cell11.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(balanceLessCommittedAmount));
			}
			else
				cell11.setCellValue(" ");
			cellNumber++;
		}
		cellNumber = 1;
		Row totalRow = sheet.createRow(rowNumber++);
		Cell cell12 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell12.setCellValue("TOTAL");
		cellNumber = cellNumber + 2;

		Cell cell13 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell13.setCellValue(totalQuantity.doubleValue());
		cellNumber++;

		Cell cell14 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell14.setCellStyle(currencyStyle);
		cell14.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalOriginalApprovedBudget));
		cellNumber++;

		Cell cell15 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell15.setCellStyle(currencyStyle);
		cell15.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalLatestApprovedBudget));
		cellNumber++;

		Cell cell16 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell16.setCellStyle(currencyStyle);
		cell16.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalExpenditureToDate));
		cellNumber++;

		Cell cell17 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell17.setCellStyle(currencyStyle);
		cell17.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalBalance));
		cellNumber++;

		if (totalExpenditureToDate != null && totalLatestApprovedBudget != null && totalExpenditureToDate != BigDecimal.ZERO && totalLatestApprovedBudget != BigDecimal.ZERO) {
			totalUtilizationAmount = new BigDecimal(decimalFormat.format(((Double.valueOf(totalExpenditureToDate.toString()) / (Double.valueOf(totalLatestApprovedBudget.toString()))) * 100)));
		} else {
			totalUtilizationAmount = BigDecimal.ZERO;
		}
		Cell cell18 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell18.setCellStyle(currencyStyle);
		cell18.setCellValue(totalUtilizationAmount != null ? decimalFormat.format(totalUtilizationAmount) + percentage : "");
		cellNumber++;

		Cell cell19 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell19.setCellStyle(currencyStyle);
		cell19.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalCommittedAmount));
		cellNumber++;

		Cell cell20 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell20.setCellStyle(currencyStyle);
		cell20.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalBalanceLessCommittedAmount));
		cellNumber++;
	}

	private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
		Cell cell = row.createCell(cellNumber);
		cell.setCellStyle(tableBodyStyle);
		return cell;
	}

	@Override
	public String fetchAwardExpenseDetailsExtByParams(AwardExpenseDetailVO vo) {
		String awardNumber = vo.getAwardNumber();
		String accountNumber = vo.getAccountNumber();
		String internalOrderCode = vo.getInternalOrderCode();
		vo.setAwardExpenseDetailsExts(awardExpenseDao.fetchAwardExpenseDetailsExtByParams(awardNumber, accountNumber, internalOrderCode));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteAwardExpenseDetailsExtById(AwardExpenseDetailVO vo) {
		String awardNumber = vo.getAwardNumber();
		String accountNumber = vo.getAccountNumber();
		String internalOrderCode = vo.getInternalOrderCode();
		Integer awardExpenseDetailsId = vo.getAwardExpenseDetailsId();
		AwardExpenseDetailsExt awardExpenseDetailsExt = awardExpenseDao.getAwardExpenseDetailsExtById(awardExpenseDetailsId);
		awardExpenseDao.deleteAwardExpenseDetailsExtById(awardExpenseDetailsExt);
		vo.setAwardExpenseDetailsExts(awardExpenseDao.fetchAwardExpenseDetailsExtByParams(awardNumber, accountNumber, internalOrderCode));
		return commonDao.convertObjectToJSON(vo);
	}

	public List<AwardExpenseDetailVO> prepareAwardExpenseDetails(String awardNumber, String accountNumber, String type) {
		List<AwardExpenseDetailVO> awardExpenseDetailsExtVOs = new ArrayList<AwardExpenseDetailVO>();
		List <String> ioCodes = new ArrayList<>();
		try {
			ResultSet resultSet = awardExpenseDao.loadExpenseDetailsByAwardId(awardNumber, accountNumber, type);
			while (resultSet.next()) {
				String budgetCategoryTypeCode = "";
				AwardExpenseDetailVO awardExpenseDetailVO = new AwardExpenseDetailVO();
				String unAssignedTransaction = resultSet.getString("IS_UNASSIGN_TRANSACTION");
				awardExpenseDetailVO.setBudgetCategory(resultSet.getString("BUDGET_CATEGORY"));
				awardExpenseDetailVO.setLineItem(resultSet.getString("LINE_ITEM"));
				awardExpenseDetailVO.setInternalOrderCode(resultSet.getString("IO_CODE"));
				if (unAssignedTransaction != null && unAssignedTransaction.equals("Y")) {
					awardExpenseDetailVO.setUnAssignedTransaction(true);
					awardExpenseDetailVO.setAwardExpenseDetailsExts(awardExpenseDao.fetchAwardExpenseDetailsExtByParams(awardNumber, accountNumber, ioCodes));
				} else {
					awardExpenseDetailVO.setUnAssignedTransaction(false);
					awardExpenseDetailVO.setAwardExpenseDetailsExts(awardExpenseDao.fetchAwardExpenseDetailsExtByParams(awardNumber, accountNumber, awardExpenseDetailVO.getInternalOrderCode()));
					if (awardExpenseDetailVO.getInternalOrderCode() != null && !awardExpenseDetailVO.getInternalOrderCode().equals("null")) {
						ioCodes.add(awardExpenseDetailVO.getInternalOrderCode());
					}
				}
				awardExpenseDetailVO.setQuantity(resultSet.getBigDecimal("QUANTITY"));
				if (resultSet.getBigDecimal("ORIGINAL_APPROVED_BUDGET") != null) {
					awardExpenseDetailVO.setOriginalApprovedBudget(resultSet.getBigDecimal("ORIGINAL_APPROVED_BUDGET"));
				} else {
					awardExpenseDetailVO.setOriginalApprovedBudget(BigDecimal.ZERO);
				}
				if (resultSet.getBigDecimal("LATEST_APPROVED_BUDGET") != null) {
					awardExpenseDetailVO.setLatestApprovedBudget(resultSet.getBigDecimal("LATEST_APPROVED_BUDGET"));
				} else {
					awardExpenseDetailVO.setLatestApprovedBudget(BigDecimal.ZERO);
				}
				if (resultSet.getBigDecimal("EXPENDITURE_TO_DATE") != null) {
					awardExpenseDetailVO.setExpenditureToDate(resultSet.getBigDecimal("EXPENDITURE_TO_DATE"));
				} else {
					awardExpenseDetailVO.setExpenditureToDate(BigDecimal.ZERO);
				}
				if (resultSet.getBigDecimal("BALANCE") != null) {
					awardExpenseDetailVO.setBalance(resultSet.getBigDecimal("BALANCE"));
				} else {
					awardExpenseDetailVO.setBalance(BigDecimal.ZERO);
				}
				String studentHoursFlag = resultSet.getString("STUDENT_HRS_FLAG");
				if (studentHoursFlag != null && studentHoursFlag.equals("Y")) {
					awardExpenseDetailVO.setStudentHoursFlag(true);
				} else {
					awardExpenseDetailVO.setStudentHoursFlag(false);
				}
				awardExpenseDetailVO.setUtilizationRate(resultSet.getBigDecimal("UTILIZATION_RATE"));
				budgetCategoryTypeCode = resultSet.getString("BUDGET_CATEGORY_TYPE_CODE");
				awardExpenseDetailVO.setBudgetCategoryTypeCode(budgetCategoryTypeCode);
				awardExpenseDetailVO.setUpdateTimeStamp(resultSet.getTimestamp("UPDATE_TIMESTAMP"));
				String sortOrder = resultSet.getString("SORT_ORDER");
				String budgetDetailId = resultSet.getString("BUDGET_DETAILS_ID");
				if (budgetDetailId != null) {
					awardExpenseDetailVO.setBudgetDetailId(Integer.parseInt(budgetDetailId));
				}
				if (sortOrder != null) {
					awardExpenseDetailVO.setSortOrder(Integer.parseInt(sortOrder));
				}
				if (budgetCategoryTypeCode != null && budgetCategoryTypeCode.equals("P")) {
					List<AwardBudgetPersonalDetail> personsDetails = awardBudgetDao
							.getAwardBudgetPersonalDetailsByDetailId(Integer.parseInt(budgetDetailId));
					if (personsDetails != null && !personsDetails.isEmpty()) {
						for (AwardBudgetPersonalDetail personsDetail : personsDetails) {
							if (personsDetail.getInternalOrderCode() != null) {
								ioCodes.add(personsDetail.getInternalOrderCode());
								List<AwardExpenseDetailsExt> awardExpenseDetailsExts = awardExpenseDao
										.fetchAwardExpenseDetailsExtByParams(awardNumber, accountNumber,
												personsDetail.getInternalOrderCode());
								if (awardExpenseDetailsExts != null && !awardExpenseDetailsExts.isEmpty()) {
									awardExpenseDetailVO.getAwardExpenseDetailsExts().addAll(awardExpenseDetailsExts);
								}
							}
						}
					}
				}
				awardExpenseDetailsExtVOs.add(awardExpenseDetailVO);
			}
			Collections.sort(awardExpenseDetailsExtVOs, new AwardExpenseDetailsComparatorBySortOrder());
		} catch (Exception e) {
			logger.info(e.getMessage());
			return awardExpenseDetailsExtVOs;
		}
		return awardExpenseDetailsExtVOs;
	}

	@Override
	public String loadAwardExpensePersonDetails(Integer budgetDetailId, String awardNumber, String accountNumber) {
		List<AwardExpenseDetailVO> awardExpenseDetailsExtVOs = new ArrayList<>();
		try {
			ResultSet resultSet = awardExpenseDao.loadAwardExpensePersonDetails(budgetDetailId, awardNumber, accountNumber);
			while (resultSet.next()) {
				String budgetCategoryTypeCode = "";
				AwardExpenseDetailVO awardExpenseDetailVO = new AwardExpenseDetailVO();
				awardExpenseDetailVO.setBudgetCategory(resultSet.getString("BUDGET_CATEGORY"));
				awardExpenseDetailVO.setLineItem(resultSet.getString("LINE_ITEM"));
				awardExpenseDetailVO.setInternalOrderCode(resultSet.getString("IO_CODE"));
				awardExpenseDetailVO.setPersonName(resultSet.getString("PERSON_NAME"));
				awardExpenseDetailVO.setAwardExpenseDetailsExts(awardExpenseDao.fetchAwardExpenseDetailsExtByParams(awardNumber, accountNumber, awardExpenseDetailVO.getInternalOrderCode()));
				awardExpenseDetailVO.setQuantity(resultSet.getBigDecimal("QUANTITY"));
				if (resultSet.getBigDecimal("ORIGINAL_APPROVED_BUDGET") != null) {
					awardExpenseDetailVO.setOriginalApprovedBudget(resultSet.getBigDecimal("ORIGINAL_APPROVED_BUDGET"));
				} else {
					awardExpenseDetailVO.setOriginalApprovedBudget(BigDecimal.ZERO);
				}
				if (resultSet.getBigDecimal("LATEST_APPROVED_BUDGET") != null) {
					awardExpenseDetailVO.setLatestApprovedBudget(resultSet.getBigDecimal("LATEST_APPROVED_BUDGET"));
				} else {
					awardExpenseDetailVO.setLatestApprovedBudget(BigDecimal.ZERO);
				}
				if (resultSet.getBigDecimal("EXPENDITURE_TO_DATE") != null) {
					awardExpenseDetailVO.setExpenditureToDate(resultSet.getBigDecimal("EXPENDITURE_TO_DATE"));
				} else {
					awardExpenseDetailVO.setExpenditureToDate(BigDecimal.ZERO);
				}
				if (resultSet.getBigDecimal("BALANCE") != null) {
					awardExpenseDetailVO.setBalance(resultSet.getBigDecimal("BALANCE"));
				} else {
					awardExpenseDetailVO.setBalance(BigDecimal.ZERO);
				}
				String studentHoursFlag = resultSet.getString("STUDENT_HRS_FLAG");
				if (studentHoursFlag != null && studentHoursFlag.equals("Y")) {
					awardExpenseDetailVO.setStudentHoursFlag(true);
				} else {
					awardExpenseDetailVO.setStudentHoursFlag(false);
				}
				awardExpenseDetailVO.setUtilizationRate(resultSet.getBigDecimal("UTILIZATION_RATE"));
				budgetCategoryTypeCode = resultSet.getString("BUDGET_CATEGORY_TYPE_CODE");
				awardExpenseDetailVO.setBudgetCategoryTypeCode(budgetCategoryTypeCode);
				String sortOrder = resultSet.getString("SORT_ORDER");
				if (sortOrder != null) {
					awardExpenseDetailVO.setSortOrder(Integer.parseInt(sortOrder));
				}
				awardExpenseDetailsExtVOs.add(awardExpenseDetailVO);
			}
			Collections.sort(awardExpenseDetailsExtVOs);
		} catch (Exception e) {
			logger.info(e.getMessage());
			return commonDao.convertObjectToJSON(awardExpenseDetailsExtVOs);
		}
		return commonDao.convertObjectToJSON(awardExpenseDetailsExtVOs);
	}

	@Override
	public String loadExpenseDetailsBasedOnCostElement(String awardNumber, String accountNumber, String type) {
		List<AwardExpenseDetailVO> awardExpenseDetailsExtVOs = new ArrayList<>();
		try {
			ResultSet resultSet = awardExpenseDao.loadExpenseDetailsAsGroup(awardNumber, accountNumber, type);
			while (resultSet.next()) {
				String budgetCategoryTypeCode = "";
				String costElementCode = resultSet.getString("COST_ELEMENT");
				Integer budgetHeaderId = Integer.parseInt(resultSet.getString("BUDGET_HEADER_ID"));
				AwardExpenseDetailVO awardExpenseDetailVO = new AwardExpenseDetailVO();
				awardExpenseDetailVO.setBudgetCategory(resultSet.getString("BUDGET_CATEGORY"));
				awardExpenseDetailVO.setLineItem(resultSet.getString("LINE_ITEM"));
				awardExpenseDetailVO.setQuantity(resultSet.getBigDecimal("QUANTITY"));
				awardExpenseDetailVO.setInternalOrderCode(resultSet.getString("IO_CODE"));
				if (resultSet.getBigDecimal("ORIGINAL_APPROVED_BUDGET") != null) {
					awardExpenseDetailVO.setOriginalApprovedBudget(resultSet.getBigDecimal("ORIGINAL_APPROVED_BUDGET"));
				} else {
					awardExpenseDetailVO.setOriginalApprovedBudget(BigDecimal.ZERO);
				}
				if (resultSet.getBigDecimal("LATEST_APPROVED_BUDGET") != null) {
					awardExpenseDetailVO.setLatestApprovedBudget(resultSet.getBigDecimal("LATEST_APPROVED_BUDGET"));
				} else {
					awardExpenseDetailVO.setLatestApprovedBudget(BigDecimal.ZERO);
				}
				if (resultSet.getBigDecimal("EXPENDITURE_TO_DATE") != null) {
					awardExpenseDetailVO.setExpenditureToDate(resultSet.getBigDecimal("EXPENDITURE_TO_DATE"));
				} else {
					awardExpenseDetailVO.setExpenditureToDate(BigDecimal.ZERO);
				}
				if (resultSet.getBigDecimal("BALANCE") != null) {
					awardExpenseDetailVO.setBalance(resultSet.getBigDecimal("BALANCE"));
				} else {
					awardExpenseDetailVO.setBalance(BigDecimal.ZERO);
				}
				String studentHoursFlag = resultSet.getString("STUDENT_HRS_FLAG");
				if (studentHoursFlag != null && studentHoursFlag.equals("Y")) {
					awardExpenseDetailVO.setStudentHoursFlag(true);
				} else {
					awardExpenseDetailVO.setStudentHoursFlag(false);
				}
				awardExpenseDetailVO.setUtilizationRate(resultSet.getBigDecimal("UTILIZATION_RATE"));
				budgetCategoryTypeCode = resultSet.getString("BUDGET_CATEGORY_TYPE_CODE");
				awardExpenseDetailVO.setBudgetCategoryTypeCode(budgetCategoryTypeCode);
				String sortOrder = resultSet.getString("SORT_ORDER");
				if (sortOrder != null) {
					awardExpenseDetailVO.setSortOrder(Integer.parseInt(sortOrder));
				}
				List<AwardBudgetDetail> awardBudgetDetails = awardBudgetDao.getAwardBudgetDetailsByParams(budgetHeaderId, costElementCode);
				if (awardBudgetDetails != null && !awardBudgetDetails.isEmpty()) {
					for (AwardBudgetDetail awardBudgetDetail : awardBudgetDetails) {
						if (awardBudgetDetail.getInternalOrderCode() != null) {
							awardExpenseDetailVO.getAwardExpenseDetailsExts().addAll(awardExpenseDao.fetchAwardExpenseDetailsExtByParams(awardNumber, accountNumber, awardBudgetDetail.getInternalOrderCode()));
						}
						if (awardBudgetDetail.getBudgetCategory().getBudgetCategoryTypeCode().equals("P")) {
							List<AwardBudgetPersonalDetail> personsDetails = awardBudgetDao.getAwardBudgetPersonalDetailsByDetailId(awardBudgetDetail.getBudgetDetailId());
							if (personsDetails != null && !personsDetails.isEmpty()) {
								for (AwardBudgetPersonalDetail personsDetail : personsDetails) {
									if (personsDetail.getInternalOrderCode() != null) {
										List<AwardExpenseDetailsExt> awardExpenseDetailsExts = awardExpenseDao.fetchAwardExpenseDetailsExtByParams(awardNumber, accountNumber, personsDetail.getInternalOrderCode());
										if (awardExpenseDetailsExts != null && !awardExpenseDetailsExts.isEmpty()) {
											awardExpenseDetailVO.getAwardExpenseDetailsExts().addAll(awardExpenseDetailsExts);
										}
									}
								}
							}
						}
					}
				}
				awardExpenseDetailsExtVOs.add(awardExpenseDetailVO);
			}
			Collections.sort(awardExpenseDetailsExtVOs);
		} catch (Exception e) {
			logger.info(e.getMessage());
			return commonDao.convertObjectToJSON(awardExpenseDetailsExtVOs);
		}
		return commonDao.convertObjectToJSON(awardExpenseDetailsExtVOs);
	}

}
