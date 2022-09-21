package com.polus.fibicomp.dashboard.service;

import java.io.IOException;
import java.util.List;

import javax.validation.Valid;

import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.dashboard.vo.AgreementDashboardVO;
import com.polus.fibicomp.dashboard.vo.AwardDashboardVO;
import com.polus.fibicomp.dashboard.vo.ClaimDashboardVO;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.dashboard.vo.GrantCallDashboardVO;
import com.polus.fibicomp.dashboard.vo.InstituteProposalDashboardVO;
import com.polus.fibicomp.dashboard.vo.NegotiationDashboardVO;
import com.polus.fibicomp.dashboard.vo.ProgressReportDashboardVO;
import com.polus.fibicomp.dashboard.vo.ProposalDashboardVO;
import com.polus.fibicomp.dashboard.vo.ServiceRequestDashboardVO;
import com.polus.fibicomp.vo.CommonVO;

/**
 * Dashboard Service class to get dashboard details.
 *
 */
@Service
public interface DashboardService {

	/**
	 * This method is used to retrieve dashboard data based on tabIndex.
	 * @param vo - Object of CommonVO
	 * @return A list of dashboard data based on tabIndex.
	 * @throws Exception
	 */
	public String getDashBoardData(CommonVO vo) throws Exception;

	/**
	 * This method is used to get XSSFWorkbook based on index tab clicked in dashboard.
	 * @param vo - object of CommonVO.
	 * @param XSSFWorkbook for excel sheet preparation.
	 * @return XSSFWorkbookthat contains excel sheet with data.
	 * @throws Exception
	 */
	public XSSFWorkbook getXSSFWorkbookForDashboard(CommonVO vo) throws Exception;

	/**
	 * This method is used to get excel sheet in byte array format.
	 * @param XSSFWorkbook for excel sheet.
	 * @return ResponseEntity<byte[]> that contains data in byte array.
	 * @throws Exception
	 */
	public ResponseEntity<byte[]> getResponseEntityForDownload(CommonVO vo, XSSFWorkbook workbook) throws Exception;

	/**
	 * This method is used to Fetch all parameters.
	 * @return A String of details of flags.
	 * @throws Exception
	 */
	public String fetchRequiredParams() throws Exception;

	/**
	 * This method is used to fetch EvaluationStop.
	 * @params commonVO object.
	 * @return evaluation stop details
	 */
	public String fetchEvaluationStop(CommonVO vo);

	/**
	 * This method is used to generate pdf file byte array.
	 * @params documentHeading.
	 * @params workbook
	 * @return pdf byte array
	 */
	public byte[] generatePDFFileByteArray(String documentHeading, XSSFWorkbook workbook);

	public void prepareExcelSheet(List<Object[]> dashboardData, XSSFSheet sheet, Object[] tableHeadingRow,XSSFWorkbook workbook, CommonVO vo);

	/**
	 * This method is used to retrieve agreement dashboard data .
	 * 
	 * @param vo - Object of AgreementDashboardVO
	 * @return A list of dashboard agreement data.
	 * @throws Exception
	 */
	public String getAgreementDashBoardData(AgreementDashboardVO vo) throws Exception;

	/**
	 * This method is used to retrieve negotiation dashboard data .
	 * 
	 * @param vo - Object of NegotiationDashboardVO
	 * @return A list of dashboard negotiation data.
	 * @throws Exception
	 */
	public String getNegotaiationDashBoardData(NegotiationDashboardVO vo) throws Exception;

	/**
	 * This method is used to retrieve grantcall dashboard data .
	 * 
	 * @param vo - Object of GrantCallDashboardVO
	 * @return A list of dashboard grantCall data.
	 * @throws Exception
	 */
	public String getGrantCallDashBoardData(GrantCallDashboardVO vo) throws Exception;

	/**
	 * This method is used to retrieve instituteProposal dashboard data .
	 * 
	 * @param vo - Object of InstituteProposalDashboardVO
	 * @return A list of dashboard instituteProposal data.
	 * @throws Exception
	 */
	public String getInstituteProposalDashBoardData(InstituteProposalDashboardVO vo) throws Exception;

	/**
	 * This method is used to retrieve award dashboard data .
	 * 
	 * @param vo - Object of AwardDashboardVO
	 * @return A list of dashboard award data.
	 * @throws Exception
	 */
	public String getAwardDashBoardData(AwardDashboardVO vo) throws Exception;

	/**
	 * This method is used to get excel sheet in byte array format.
	 * 
	 * @param XSSFWorkbook for excel sheet.
	 * @return ResponseEntity<byte[]> that contains data in byte array.
	 * @throws Exception
	 */
	public ResponseEntity<byte[]> getResponseEntityForAwardDownload(AwardDashboardVO vo, XSSFWorkbook workbook) throws Exception;

	/**
	 * This method is used to get XSSFWorkbook based on index tab clicked in dashboard.
	 * 
	 * @param vo- object of AwardDashboardVO.
	 * @param XSSFWorkbook for excel sheet preparation.
	 * @return XSSFWorkbookthat contains excel sheet with data.
	 * @throws Exception
	 */
	public XSSFWorkbook getXSSFWorkbookForAwardDashboard(AwardDashboardVO vo) throws Exception;

	/**
	 * This method is used to retrieve award dashboard data .
	 * 
	 * @param vo - Object of AwardDashboardVO
	 * @return A list of dashboard award data.
	 * @throws Exception
	 */
	public String getProposalDashBoardData(ProposalDashboardVO vo) throws Exception;

	/**
	 * This method is used to get excel sheet in byte array format.
	 * 
	 * @param XSSFWorkbook for excel sheet.
	 * @return ResponseEntity<byte[]> that contains data in byte array.
	 * @throws Exception
	 */
	public ResponseEntity<byte[]> getResponseEntityForProposalDownload(ProposalDashboardVO vo, XSSFWorkbook workbook) throws Exception;

	/**
	 * This method is used to get XSSFWorkbook based on index tab clicked in dashboard.
	 * 
	 * @param vo - object of ProposalDashboardVO.
	 * @param XSSFWorkbook for excel sheet preparation.
	 * @return XSSFWorkbookthat contains excel sheet with data.
	 * @throws Exception
	 */
	public XSSFWorkbook getXSSFWorkbookForProposalDashboard(ProposalDashboardVO vo) throws Exception;

	/**
	 * This method is used to load the service request dashboard.
	 * @param vo
	 * @return list of service requests
	 * @throws Exception
	 */
	public String loadServiceRequestDashBoard(ServiceRequestDashboardVO vo) throws Exception;

	/**
	 * This method is used to generate workbook for the service request dashboard
	 * @param vo
	 * @return XSSFWorkbook of exported data
	 * @throws Exception
	 */
	public XSSFWorkbook getXSSFWorkbookServiceRequestDashBoard(ServiceRequestDashboardVO vo) throws Exception;

	/**
	 * This method is used to 
	 * @param vo
	 * @param workbook
	 * @return ResponseEntity<byte[]> that contains data in byte array 
	 * @throws IOException 
	 */
	public ResponseEntity<byte[]> getResponseEntityServiceRequestDashBoard(ServiceRequestDashboardVO vo, XSSFWorkbook workbook) throws IOException;

	/**
	 * This method is used to export institute proposal dashboard datas
	 * @param vo
	 * @return object of XSSFWorkbook
	 */
	public XSSFWorkbook getXSSFWorkbookForInstituteProposalDashboard(InstituteProposalDashboardVO vo);

	/**
	 * This method is used to check user can DeleteGrantCall
	 * @param vo
	 * @return String reponse
	 */
	public String canDeleteGrantCall(GrantCallDashboardVO vo);

	/**	 
	 * * This method is used to get claim dashboard data
	 * @param vo
	 * @return A list of claim dashboard data.
	 */
	public String getClaimDashBoardData(ClaimDashboardVO vo);

	/**
	 * @param vo
	 * @return list of award progress report data
	 */
	public String fibiProgressReportDashBoard(ProgressReportDashboardVO vo);

	public XSSFWorkbook getXSSFWorkbookForGrantCallDashboard(GrantCallDashboardVO vo);

	ResponseEntity<byte[]> getResponseEntityForDownloadExpense(CommonVO vo, XSSFWorkbook workbook) throws Exception;

	/**
	 * This method is used to get Agreement Based On Category .
	 * @param vo - Object of DashBoardProfile
	 * @return A list of dashboard Agreement data.
	 */
	public String getAgreementBasedOnCategory(AgreementDashboardVO vo);

	/**
	 * This method is used to get XSSFWorkbook For AgreementDashboard .
	 * @param vo - Object of AgreementDashboardVO
	 * @return A list of dashboard Agreement data.
	 */
	public XSSFWorkbook getXSSFWorkbookForAgreementDashboard(AgreementDashboardVO vo);

	/**
	 * This method is used to get Agreement Based On Category .
	 * @param vo - Object of AgreementDashboardVO
	 * @return XSSF workBook.
	 */
	public XSSFWorkbook getXSSFWorkbookForAgreementCategoryDashboard(AgreementDashboardVO vo);

	/**
	 * This method is used to get COI dasboard data .
	 * @param vo - 
	 * @return A list of dashboard COI data.
	 */
	public String getCOIDashboard(CoiDashboardVO vo);

	/**
	 * This method is used to get COI Admin dasboard data .
	 * @param vo - 
	 * @return A list of dashboard COI data.
	 */
	public String getCOIAdminDashboard(@Valid CoiDashboardVO vo);

	/**
	 * This method is used to get SFI dasboard data .
	 * @param vo - 
	 * @return A list of dashboard SFI data.
	 */
	public String getSFIDashboard(CoiDashboardVO vo);
  
}
