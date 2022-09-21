package com.polus.fibicomp.common.dao;

import java.sql.Timestamp;
import java.text.ParseException;
import java.util.Date;
import java.util.List;

import com.polus.fibicomp.pojo.*;
import com.polus.fibicomp.pojo.Module;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.agreements.pojo.AdminGroup;
import com.polus.fibicomp.agreements.pojo.AgreementNoteFileData;
import com.polus.fibicomp.agreements.pojo.SponsorRole;
import com.polus.fibicomp.award.pojo.AccountType;
import com.polus.fibicomp.award.pojo.CostShareType;
import com.polus.fibicomp.customdataelement.pojo.LookupWindow;
import com.polus.fibicomp.evaluation.pojo.ReviewComment;
import com.polus.fibicomp.negotiation.pojo.NegotiationCommentFileData;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.proposal.pojo.CommentType;
import com.polus.fibicomp.proposal.pojo.CongressionalDistrict;
import com.polus.fibicomp.proposal.pojo.NarrativeStatus;
import com.polus.fibicomp.proposal.pojo.ProposalAttachmentType;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.vo.LookUp;

@Transactional
@Service
public interface CommonDao {

	/**
	 * This method is used to get next sequence number.
	 * 
	 * @param sequenceName - name of the sequence.
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
	 * This method is used to parameter value.
	 * 
	 * @param namespaceCode - Namespace code of the parameter.
	 * @param componentCode - Component code of the parameter.
	 * @param parameterName - Name of the parameter.
	 * @return value of parameter.
	 */
	public Integer getParameter(String parameterName);

	/**
	 * This method is used to parameter value as string.
	 * 
	 * @param namespaceCode - Namespace code of the parameter.
	 * @param componentCode - Component code of the parameter.
	 * @param parameterName - Name of the parameter.
	 * @return value of parameter.
	 */
	public String getParameterValueAsString(String parameterName);

	/**
	 * This method is used to get document status based on id.
	 * 
	 * @param documentStatusCode - Id of the DocumentStatus.
	 * @return DocumentStatus.
	 */
	public DocumentStatus getDocumentStatusById(Integer documentStatusCode);

	/**
	 * This method is used to save FileData object.
	 * 
	 * @param fileData - FileData object.
	 * @return FileData.
	 */
	public FileData saveFileData(FileData fileData);

	/**
	 * This method is used to fetch FileData object.
	 * 
	 * @param fileData - FileData object.
	 * @return FileData.
	 */
	public FileData getFileDataById(String fileDataId);

	/**
	 * This method is used to delete FileData object.
	 * 
	 * @param fileData - FileData object.
	 */
	public void deleteFileData(FileData fileData);

	/**
	 * This method is used to convert Object into JSON format.
	 * 
	 * @param object - request object.
	 * @return response - JSON data.
	 */
	public String convertObjectToJSON(Object object);

	/**
	 * this method is used for get current date
	 * 
	 * @return current date
	 */
	public Date getCurrentDate();

	/**
	 * this method is used for finding the number of days
	 * 
	 * @input is start date and end date
	 * @output is end date-start date+1
	 **/
	public Long getNumberOfDays(Date startDate, Date endDate);

	/**
	 * this method is used get unit name
	 * 
	 * @input unit number
	 * @output unit name
	 **/
	public String getUnitName(String unitNumber);

	/**
	 * this method is used to get FAQ questions and answers as list
	 * 
	 * @return
	 */
	public List<Faq> fibiFaq();

	/**
	 * Author Shaji P This method is to fetch the lead unit by lead unit number
	 * 
	 * @param leadUnitNumber
	 * @return Lead Unit Object
	 */

	public Unit getLeadUnitByUnitNumber(String leadUnitNumber);

	/**
	 * This method is used to retrieve current Timestamp.
	 * 
	 * @return current Timestamp.
	 */

	public Timestamp getCurrentTimestamp();

	/**
	 * Get sponsor code for a sponsor Name
	 * 
	 * @return
	 */

	public String getSponsorName(String sponorCode);

	/**
	 * This method is used to delete review comment.
	 * 
	 * @param commentId - commentId.
	 */
	public void deleteReviewComment(ReviewComment reviewComment);

	/**
	 * This method is used to get Unit by unit number.
	 * 
	 * @param unitNumber
	 */
	public Unit getUnitByUnitNumber(String unitNumber);

	/**
	 * this method is used get unit name
	 * 
	 * @input unit number
	 * @output unit name
	 **/
	public String getUnitAcronym(String unitNumber);

	/**
	 * 
	 * @param date
	 * @return
	 */
	public Date adjustTimezone(Date date);

	/***
	 * This method will convert Java Object to JSON, Please use this method for
	 * conversion small objects for less than 10 Kb, best for lookup data and for
	 * some search results
	 * 
	 * @param object
	 * @return
	 */
	public String convertToJSON(Object object);

	/**
	 * This method will return argument data for an input argument name
	 * 
	 * @param argumentName
	 * @return
	 */
	public List<ArgValueLookup> getArgValueLookupData(String argumentName);

	/**
	 * This method is used to fetch all activity types.
	 * 
	 * @return A list of activity types.
	 */
	public List<ActivityType> fetchAllActivityTypes();

	/**
	 * This method is used to fetch all units.
	 * 
	 * @return A list of units.
	 */
	public List<Unit> fetchAllUnits();

	/**
	 * This method is used to fetch all NarrativeStatus.
	 * 
	 * @return A list of NarrativeStatus.
	 */
	public List<NarrativeStatus> fetchAllNarrativeStatus();

	/**
	 * This method is used to fetch Sponsor by sponsorCode.
	 * 
	 * @return A Object of Sponsor.
	 */
	public Sponsor getSponsorById(String sponsorCode);

	public List<AccountType> fetchAllAccountTypes();

	public List<SpecialReviewType> fetchAllReviewTypes();

	public List<SpecialReviewApprovalType> fetchAllApprovalStatusTypes();

	public List<CostShareType> fetchAllCostShareTypes();

	/**
	 * @param searchString
	 * @return List of Organizations
	 */
	public List<Organization> fetchOrganizationList(String searchString);

	/**
	 * @param searchString
	 * @return List of Country
	 */
	public List<Country> fetchCountryList(String searchString);

	/**
	 * This method is used to get account type based on account type code
	 * @param accountTypeCode
	 * @return accountType
	 */
	public AccountType fetchAccountTypeByAccountTypeCode(Integer accountTypeCode);

	/**
	 * This method is used to get activity type based on activity type code
	 * @param activityTypeCode
	 * @return activityType
	 */
	public ActivityType fetchActivityTypeByActivityTypeCode(String activityTypeCode);

	/**
	 * This method is used to get cost share based on cost share type id
	 * @param costShareTypeCode
	 * @return costShareType
	 */
	public CostShareType fetchCostShareTypeByCostShareTypeCode(Integer costShareTypeCode);

	/**
	 * This method is used to get special review approval type based on special review approval type code
	 * @param specialReviewApprovalTypeCode
	 * @return specialReviewApprovalType
	 */
	public SpecialReviewApprovalType fetchSpecialReviewApprovalTypeByTypeCode(String specialReviewApprovalTypeCode);

	/**
	 * This method is used to get proposal attachment type.
	 * @param attachmentTypeCode - Id of the ProposalAttachmentType.
	 * @return ProposalAttachmentType.
	 */
	public ProposalAttachmentType getProposalAttachmentType(Integer attachmentTypeCode);

	/**
	 * This method is used to get narrative status based on id.
	 * @param code - Id of the NarrativeStatus.
	 * @return NarrativeStatus.
	 */
	public NarrativeStatus getNarrativeStatusByCode(String code);

	/**
	 * This method is used to fetch Role by roleId.
	 * @return A Object of Role.
	 */
	public Role getRoleById(Integer roleId);

	/**
	 * This method is used to get lookup datas based on table name and its primary key column name 
	 * @param lookUpTableName
	 * @param lookUpTableColumnName
	 * @return list of look up datas
	 */
	public List<LookUp> getLookUpDatas(String lookUpTableName, String lookUpTableColumnName);

	/**
	 * This method is used to fetch date format.
	 * @return date in string.
	 */
	public String getDateFormat(Date date, String dateFormat);

	/**
	 * This method is used fetch Currency Details.
	 * @return Currency Details.
	 */
	public List<Currency> fetchCurrencyDetails();

	/**
	 * This method is used fetch get Module Details.
	 * @return Module Details.
	 */
	public List<Module> getModules();

	/**
	 * This method is used fetch Currency Details Based on ID.
	 * @return Currency Details Based on id.
	 */
	public Currency getCurrencyByCurrencyCode(String currencyCode);

	/**
	 * This method is used to get filtered module list.
	 * @param moduleCodes - moduleCodes
	 * @return filtered modules list.
	 */
	public List<Module> getFilteredModules(List<Integer> moduleCodes);

	/**
	 * This method is used to get filtered module list.
	 * @return filtered modules list.
	 */
	public Integer fetchMaxOrganizationId();

	/**
	 * This method is used to fetch
	 * @param moduleCode
	 * @return
	 */
	public List<CommentType> fetchCommentTypes(Integer moduleCode);

	/**
	 * @param arrayList
	 * @return list of person details
	 */
	public List<Person> getPersonDetailByUserName(List<String> userNames);

	/**
	 * @param arrayList
	 * @return list of person details
	 */
	public List<Person> getPersonDetailByPersonId(List<String> personId);

	/**
	 * @param rolodexId
	 * @return list of person details
	 */
	public List<Rolodex> getRolodexDetailByRolodexId(List<Integer> rolodexId);

	/**
	 * This method is used to retrieve current time based on timezone.
	 * @param timeZone
	 * @return
	 * @throws ParseException
	 */
	public Timestamp getCurrentDateBasedOnTimeZone(String timeZone) throws ParseException;

	/**
	 * This method is used save or update the Organization
	 * @param organization
	 * @return updated organization
	 */
	public void saveOrUpdateOrganization(Organization organization);

	/**
	 * This method is used check uniqueness of given organization ID
	 * @param organizationId
	 * @return boolean
	 */
	public boolean checkIsOrganizationIdUnique(String organizationId);

	/**
	 * This method is used get organization details
	 * @param organizationId
	 * @return Organization
	 */
	public Organization loadOrganizationDetails(String organizationId);

	/**
	 * This method is used get organization of a unit
	 * @param unitNumber
	 */
	public String getOrganizationOfUnit(String unitNumber);

	/**
	 * @param entity
	 */
	public void detachEntityFromSession(Object entity);

	/**
	 * @param searchString
	 * @return list of CongressionalDistrict
	 */
	public List<CongressionalDistrict> findCongressionalDistricts(String searchString);

	/**
	 * This method is used fetch All ResearchTypes
	 * @return List of ResearchType
	 */
	public List<ResearchType> fetchAllResearchTypes();

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
	 * @param moduleCode
	 * @param moduleItemId 
	 * @param rightName
	 * @param loginPersonId
	 * @return boolean
	 */
	public Boolean checkPersonHasRightInModule(Integer moduleCode, Integer moduleItemId, List<String> rightName, String loginPersonId);
	
  	/**
	 * This method is used to fetch the lookup datas that are defined by the user
	 * @return list of user defined lookups
	 */
	public List<String> getUserDeffinedLookup();

	/**
	 * This method is used to get all the lookup windows
	 * @return list of lookup windows
	 */
	public List<LookupWindow> getAllLookUpWindows();

	/**
	 * @return
	 */
	public Timestamp getStartTimeOfCurrentDay();
	
  /**
   * This method is used fetch different Modules.
	 * @return Module Details.
	 */
	public List<AgreementLinkModule> fetchModules();

	/**
	 * This method is used to get sponsor role details by roleTypeCode.
	 * @param roleTypeCode
	 * @return
	 */
	public SponsorRole getSponsorRoleByRoleTypeCode(String roleTypeCode);
	
	/**
	 * This method is used to find roles
	 * @param searchString - searchString .
	 * @return - role list
	 */
	public List<Role> findRole(String searchString);

	/**
	 * This method is used to save FileData object.
	 * @param fileData - FileData object.
	 * @return FileData.
	 */
	public AgreementNoteFileData saveFileData(AgreementNoteFileData agreementNoteFileData);

	/**
	 * This method is used to fetch FileData object.
	 * @param fileData - FileData object.
	 * @return FileData.
	 */
	public NegotiationCommentFileData getNegotiationCommentFileDataById(String fileDataId);

	/**
	 * This method is used to fetch FileData object.
	 * @param fileData - FileData object.
	 * @return FileData.
	 */
	public AgreementNoteFileData getAgreementNoteFileDataById(String fileDataId);

	/**
	 * This method is used to get the web socket configuration datas based on the configuration key
	 * @param parameterName
	 * @return configuration value
	 */
	WebSocketConfiguration getWebSocketConfigurationValue(String configKey);

	/**
	 * @param moduleCode
	 * @param moduleItemKey
	 * @param property1
	 * @param notificationTypeCode
	 * @return List<Object[]>
	 */
	public List<Object[]> getNotificationLogForPerson(Integer moduleCode, String moduleItemKey,
			String mailAddress, Integer notificationTypeCode);

	
	/**
	 * This method is used to convert timestamp to specific time zone and date format
	 *
	 * @param timeZone
	 * @param dateFormat
	 * @return (String)date
	 */
	String getDateFromTimestampZoneFormat(String timeZone, String dateFormat);

	/**
	 * @param moduleItemKey
	 * @return
	 */
	public Object[] fetchModuleDetailsBasedOnId(Integer moduleCode, Integer moduleItemKey);

	String getAdminGroupEmailAddressByAdminGroupId(Integer adminGroupId);

	List<AdminGroup> fetchAdminGroupsBasedOnModuleCode(Integer moduleCode);

	AdminGroup getAdminGroupByGroupId(Integer adminGroupId);

	List<Integer> getAdminGroupIdsBasedOnPersonId(String personId);
	
	/**
	 * This method is used to fetch all the countryName and countryCode
	 * @return list of country details.
	 */
	public List<Country> getCountryLookUp();

	/**
	 * This methode used to get Letter Template by template type code
	 *
	 * @param templateTypeCode
	 * @return LetterTemplateType
	 */
	LetterTemplateType getLetterTemplate(String templateTypeCode);

	/**
	 * This methode used to get Letter Template by module code
	 *
	 * @param moduleCode
	 * @return list of LetterTemplateType
	 */
	List<Object> getAllLetterTemplateTypes(Integer moduleCode);

	/**
	 * This methode used to get Letter Template
	 *
	 * @param searchKeyword Search Keyword
	 * @return list of LetterTemplateType
	 */
	List<Object> fetchAllLetterTemplateTypes(String searchKeyword);

	/**
	 * This method is used to sync the current transaction with the database
	 */
	public void doflush();

	/**
	 * This method is used to check the dyn sub section is enabled or not
	 */
	public boolean isDynSubSectionEnabled(String subSectionCode);
}
