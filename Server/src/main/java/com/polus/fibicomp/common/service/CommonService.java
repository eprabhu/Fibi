package com.polus.fibicomp.common.service;

import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipOutputStream;

import javax.servlet.http.HttpServletRequest;

import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.itextpdf.text.pdf.PdfWriter;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;
import com.polus.fibicomp.pojo.ArgValueLookup;
import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.ResearchTypeArea;
import com.polus.fibicomp.pojo.ResearchTypeSubArea;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.LookUp;

import io.jsonwebtoken.Claims;

@Transactional
@Service
public interface CommonService {

    /**
     * This method is used to get next sequence number.
     *
     * @param sequenceName - Name of the sequence.
     * @return sequence number.
     */
    public Long getNextSequenceNumber(String sequenceName);
  
    /**
     * This method is used to parameter value as boolean.
     *
     * @param namespaceCode - Namespace code of the parameter.
     * @param componentCode - Component code of the parameter.
     * @param parameterName - Name of the parameter.
     * @return boolean value of parameter.
     */
    public boolean getParameterValueAsBoolean(String parameterName);

    /**
     * This method is used to get current fiscal year.
     *
     * @return fiscal year.
     */
    public Integer getCurrentFiscalYear();

    /**
     * This method is used to get current fiscal month.
     *
     * @return fiscal month.
     */
    public Integer getCurrentFiscalMonthForDisplay();

    /**
     * This method is used to create encrypted password.
     *
     * @param valueToHide - Password to encrypt
     * @return Encrypted password
     * @throws GeneralSecurityException
     */
    public String hash(Object valueToHide) throws GeneralSecurityException;

    /**
     * @param text
     * @param replacementParameters
     * @return replaced text
     */
    public String render(String text, Map<String, String> replacementParameters);

    /**
     * This method is used to send the details via mail
     *
     * @param notificationType
     * @param moduleTypeId
     * @param module
     */
    public void sendMailAtEvent(NotificationType notificationType, Integer moduleTypeId, String module);

    /**
     * this method is used for frequently asked question section
     *
     * @return
     */
    public String fibiFaq();

    /**
     * This method is used to send the mail for assign review
     *
     * @param notificationType
     * @param proposal
     * @param proposalReview
     * @param grantCallMap
     */
    public void sendMailForAssignReview(NotificationType notificationType, ProposalReview proposalReview, Map.Entry<String, Integer> grantCallEntry, String reviewerRole, Proposal proposal);

    /**
     * This method is used to send the mail for notify action
     *
     * @param notificationType
     * @param emailIds
     * @param userName
     * @param comment
     * @param message
     * @param serviceRequest
     */
    public void sendMailForNotifyAction(NotificationType notificationType, Set<String> emailIds, String userName, String comment, String message, ServiceRequest serviceRequest);

    /**
     * This method will return argument data for an input argument name
     *
     * @param argumentName
     * @return
     */
    public List<ArgValueLookup> getArgValueLookupData(String argumentName);

    /**
     * This method will placeholder information for the repective fibi module
     *
     * @param moduleCode
     * @return
     */
    public String getNotificationPlaceholder(Integer moduleCode);

    /**
     * This method is used for eliminate html code appearing from editors
     *
     * @param htmlContent
     * @return
     */
    public String replaceHtmlFromEditor(String htmlContent);

    public void setNotificationRecipients(String recipient, String recipientType, Set<NotificationRecipient> dynamicEmailrecipients);

    /**
     * This method is used to get lookup datas based on table name and its primary key column name
     *
     * @param lookUpTableName
     * @param lookUpTableColumnName
     * @return list of look up datas
     */
    public String getLookUpDatas(String lookUpTableName, String lookUpTableColumnName);

    /**
     * This method is used to convert date format
     *
     * @param dateValue
     * @return date in string
     */
    public String convertDateFormatBasedOnTimeZone(Long dateValue, String dateFormat);

    public void setNotificationRecipientsforNonEmployees(String emailAddress, String recipientType, Set<NotificationRecipient> dynamicEmailrecipients);

    /**
     * This method is used to generate file from file of chunks to resolve the waf issue
     *
     * @param file
     * @param name
     * @param remaining
     * @param length
     * @param timestamp
     * @param userId
     * @param contentType
     * @return file generate from the file chunks
     */
    public MultipartFile uploadMedia(String file, String name, Integer remaining, Integer length, String timestamp,
                                     String userId, String contentType);

    /**
     * This method is used to create file in Upload folfer
     *
     * @param workbook
     * @param fileName
     * @return file
     */
    public File createfileInUploads(XSSFWorkbook workbook, String fileName);

    /**
     * This method is used to get the value based on column name
     *
     * @param entityObject
     * @param referanceColumn
     * @return value as Object
     */
    public Object getValueBasedOnColumnName(Object entityObject, String referanceColumn);

    /**
     * This method is used to rename the duplicate files in the zip folder
     *
     * @param index
     * @param fileName
     * @param zos
     * @return next index for the name
     * @throws IOException
     */
    public Integer addFilesToZipFolder(Integer index, String fileName, ZipOutputStream zos) throws IOException;

    /**
     * This method is used to get the login person detail  from token
     *
     * @param request
     * @return loginPersonDetail
     */
    public Claims getLoginPersonDetailFromJWT(HttpServletRequest request);

    /**
     * @param searchString
     * @param researchTypeCode
     * @return List of ResearchTypeArea
     */
    public List<ResearchTypeArea> findResearchTypeArea(String searchString, String researchTypeCode);

    /**
     * @param searchString
     * @param researchTypeAreaCode
     * @param researchTypeCode
     * @return List of ResearchTypeSubArea
     */
    public List<ResearchTypeSubArea> findResearchTypeSubArea(String searchString, String researchTypeAreaCode, String researchTypeCode);

    /**
     * This method is used to add header in excel file
     *
     * @param workbook
     * @param sheet
     */
    void addDetailsInHeader(XSSFWorkbook workbook, XSSFSheet sheet);

    /**
     * This method is used to add footer in pdf file
     *
     * @param writer
     * @return
     */
    PdfWriter addPdfHeaderAndFooter(PdfWriter writer);

	  /**
	  * This method is used to fetch all the lookup window details
	  * @return list of lookup windows defined in the system
	  */
	  public List<LookUp> getAllLookUpWindowDetails();

	/**
	 * This method is used to find roles
	 * @param searchString - searchString.
	 * @return - role list
	 */
	public List<Role> findRole(String searchString);

	/**
	 * This method is used to apply sponsor format by sponsor code, sponsor name and acronym.
	 * @param sponsorCode 
	 * @param sponsorName
	 * @param acronym
	 * @return sponsor detail.
	 */
	public String getSponsorFormatBySponsorDetail(String sponsorCode, String sponsorName, String sponsorAcronym);

	/**
	 * This method is used to get the web socket configuration value by configkey
	 * @param configKey
	 * @return true or false
	 */
	public boolean getWebSocketConfigurationValue(String configKey);

	/**
	 * @param commonVO
	 * @return list of mails triggered
	 */
	public String personCertificationMailLog(CommonVO commonVO);
	
	/**
	 * This method is used to fetch all the countryName and countryCode
	 * @return list of country details.
	 */
	public List<Country> getCountryLookUp();

    /**
     * This method is used to Unit Format By Sponsor Detail
     *
     * @param unitNumber
     * @param unitName
     * @return formatted string
     */
    public String getUnitFormatByUnitDetail(String unitNumber, String unitName);

	 /**
     * This method is used to Setting approver stop name placeholder value in mail.
     *
     * @param approvalStopNumber
     * @param mapId
     * @param workflowdetailId
     * @param actionType
     */
    public String getPlaceHolderDataForRouting(Integer approvalStopNumber, Integer mapId, Integer workflowdetailId);

    /**
     * This methode used to get all Letter Template types
     *
     * @param vo
     * @return list of Letter Template types
     */
    ResponseEntity<Object> getAllLetterTemplateTypes(CommonVO vo);

    /**
     * This methode used for sha-256 encription
     * @param valueToHide - Password to encrypt
     * @return Encrypted password
     * @throws GeneralSecurityException
     */
	String hashBySha(Object valueToHide) throws GeneralSecurityException;

    /**
     * This method used for checking sftp connection
     * @param emailContent 
     * @param valueToHide - Password to encrypt
     */
	public Boolean checkSFTPConnection(EmailContent emailContent);

}
