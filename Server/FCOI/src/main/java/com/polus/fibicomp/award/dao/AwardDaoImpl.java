package com.polus.fibicomp.award.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.persistence.LockModeType;
import javax.persistence.ParameterMode;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Selection;
import javax.persistence.criteria.Subquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.NonUniqueResultException;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.hibernate.internal.SessionImpl;
import org.hibernate.procedure.ProcedureCall;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.dto.AwardHierarchyDto;
import com.polus.fibicomp.award.dto.AwardSearchResult;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardAmountTransaction;
import com.polus.fibicomp.award.pojo.AwardApprovedEquipment;
import com.polus.fibicomp.award.pojo.AwardAprovedForeignTravel;
import com.polus.fibicomp.award.pojo.AwardAttachment;
import com.polus.fibicomp.award.pojo.AwardAttachmentType;
import com.polus.fibicomp.award.pojo.AwardBasisOfPayment;
import com.polus.fibicomp.award.pojo.AwardComment;
import com.polus.fibicomp.award.pojo.AwardContact;
import com.polus.fibicomp.award.pojo.AwardContactType;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardDocumentType;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.award.pojo.AwardHierarchy;
import com.polus.fibicomp.award.pojo.AwardHistoryLog;
import com.polus.fibicomp.award.pojo.AwardKPI;
import com.polus.fibicomp.award.pojo.AwardKPICriteria;
import com.polus.fibicomp.award.pojo.AwardKeyword;
import com.polus.fibicomp.award.pojo.AwardMethodOfPayment;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPersonAttachment;
import com.polus.fibicomp.award.pojo.AwardPersonRoles;
import com.polus.fibicomp.award.pojo.AwardPersonUnit;
import com.polus.fibicomp.award.pojo.AwardProjectTeam;
import com.polus.fibicomp.award.pojo.AwardReportReminder;
import com.polus.fibicomp.award.pojo.AwardReportTermRecipient;
import com.polus.fibicomp.award.pojo.AwardReportTerms;
import com.polus.fibicomp.award.pojo.AwardReportTracking;
import com.polus.fibicomp.award.pojo.AwardReportTrackingFile;
import com.polus.fibicomp.award.pojo.AwardResearchArea;
import com.polus.fibicomp.award.pojo.AwardSpecialReview;
import com.polus.fibicomp.award.pojo.AwardSponsorTerm;
import com.polus.fibicomp.award.pojo.AwardStatus;
import com.polus.fibicomp.award.pojo.AwardSubContract;
import com.polus.fibicomp.award.pojo.AwardTransactionType;
import com.polus.fibicomp.award.pojo.AwardType;
import com.polus.fibicomp.award.pojo.ContactRoleType;
import com.polus.fibicomp.award.pojo.Distribution;
import com.polus.fibicomp.award.pojo.Frequency;
import com.polus.fibicomp.award.pojo.FrequencyBase;
import com.polus.fibicomp.award.pojo.MilestoneStatus;
import com.polus.fibicomp.award.pojo.Report;
import com.polus.fibicomp.award.pojo.ReportClass;
import com.polus.fibicomp.award.pojo.ReportStatus;
import com.polus.fibicomp.award.pojo.SponsorReport;
import com.polus.fibicomp.award.pojo.SponsorTerm;
import com.polus.fibicomp.award.pojo.SponsorTermReport;
import com.polus.fibicomp.award.pojo.SponsorTermType;
import com.polus.fibicomp.award.pojo.ValidReportClass;
import com.polus.fibicomp.award.vo.AwardDetailsVO;
import com.polus.fibicomp.award.vo.AwardHierarchyVO;
import com.polus.fibicomp.award.vo.AwardSummaryDetailsVO;
import com.polus.fibicomp.award.vo.ReportTermsVO;
import com.polus.fibicomp.budget.pojo.AwardWorkflowStatus;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.pojo.SapFeedStatus;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.ip.pojo.InstituteProposalAdminDetail;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.AgreementLinkModule;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.roles.dao.RolesManagementDao;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.sectionwiseedit.pojo.ModuleVariableSection;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.view.AwardView;
import com.polus.fibicomp.vo.CommonVO;

import oracle.jdbc.OracleTypes;

@SuppressWarnings({ "deprecation", "unused" })
@Transactional
@Service(value = "awardDao")
public class AwardDaoImpl implements AwardDao {

	protected static Logger logger = LogManager.getLogger(AwardDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private RolesManagementDao rolesManagementDao;

	@Autowired
	private CommonService commonService;

	private static final String AWARD_ID = "award_id";
	private static final String AWARD_NUMBER = "award_number";
	private static final String ACCOUNT_NUMBER = "account_number";
	private static final String LEAD_UNIT_NUMBER = "lead_unit_number";
	private static final String SPONSOR_CODE = "sponsor_code";
	private static final String SPONSOR_NAME = "sponsor_name";
	private static final String ROOT_AWARD_NUMBER = "root_award_number";
	private static final String PERSON_ID = "person_id";
	private static final String FULL_NAME = "full_name";
	private static final String PRNCPL_NM = "prncpl_nm";
	private static final String UNITNAME = "unit_name";
	private static final String EMAIL_ADDRESS = "email_address";
	private static final String DESCRIPTION = "description";
	private static final String CATEGORY_CODE = "CATEGORY_CODE";
	private static final String DESC = "DESCRIPTION";
	private static final String UNIT_NAME = "UNIT_NAME";
	private static final String UNIT_NUMBER = "UNIT_NUMBER";
	private static final String PARENT_AWARD_NUMBER = "parent_award_number";
	private static final String PI_NAME = "pi_name";
	private static final String AWARDID = "awardId";
	private static final String UPDATE_TIMESTAMP = "updateTimestamp";
	private static final String AWARD_REPORT_TERMS_ID = "awardReportTermsId";
	private static final String FILE_NAME = "fileName";	
	private static final String UPDATE_USER = "updateUser";	
	private static final String UPDATETIMESTAMP = "updateTimeStamp";	
	private static final String SPONSORCODE = "sponsorCode";	
	private static final String FUNDINGSCHEMEID = "fundingSchemeId";	
	private static final String SPONSOR_TERM_TYPECODE = "sponsorTermTypeCode";	
	private static final String AWARDNUMBER = "awardNumber";	
	private static final String SEQUENCENUMBER = "sequenceNumber";	
	private static final String AWARDSEQUENCESTATUS = "awardSequenceStatus";	
	private static final String REPORTCLASSCODE = "reportClassCode";	
	private static final String REPORTCODE = "reportCode";
	private static final String STATUSCODE = "status_code";
	
	

	@Override
	public AwardDetailsVO fetchAwardSummaryData(String awardId) {
		AwardDetailsVO awardDetailsVO = new AwardDetailsVO();
		try {
			getAwardSummaryDetails(awardId, awardDetailsVO);
			getPersonDetails(awardId, awardDetailsVO);
			getSponsorContactDetails(awardId, awardDetailsVO);
			getUnitContactDetails(awardId, awardDetailsVO);
			getFundedProposalsDetails(awardId, awardDetailsVO);
			getSpecialReviewsDetails(awardId, awardDetailsVO);
		} catch (Exception e) {
			logger.info("Exception in fetchAwardSummaryData : {}", e.getMessage());
		}
		return awardDetailsVO;
	}

	@Override
	public AwardHierarchyVO fetchAwardHierarchyData(String awardNumber, String selectedAwardNumber) {
		AwardHierarchyVO awardHierarchyVO = new AwardHierarchyVO();
		getHierarchyDetails(awardNumber, awardHierarchyVO, selectedAwardNumber);
		logger.info("awardHierarchyVO : {}", awardHierarchyVO);
		return awardHierarchyVO;
	}

	public void getAwardSummaryDetails(String awardId, AwardDetailsVO awardDetailsVO) throws Exception {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<HashMap<String, Object>> awardDetails = new ArrayList<>();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call get_award_details(?)}");
				statement.setInt(1, Integer.parseInt(awardId));
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "get_award_details";
				String functionCall = "{call " + procedureName + "(?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setString(2, awardId);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset != null && rset.next()) {
				HashMap<String, Object> detailsField = new HashMap<>();
				detailsField.put(AWARD_ID, rset.getString(AWARD_ID));
				detailsField.put(AWARD_NUMBER, rset.getString(AWARD_NUMBER));
				detailsField.put(ACCOUNT_NUMBER, rset.getString(ACCOUNT_NUMBER));
				detailsField.put("document_number", rset.getString("document_number"));
				detailsField.put("activity_type_code", rset.getString("activity_type_code"));
				detailsField.put("activity_type", rset.getString("activity_type"));
				detailsField.put("award_type_code", rset.getString("award_type_code"));
				detailsField.put("award_type", rset.getString("award_type"));
				detailsField.put("account_type_code", rset.getString("account_type_code"));
				detailsField.put("account_type", rset.getString("account_type"));
				detailsField.put("sponsor_award_number", rset.getString("sponsor_award_number"));
				detailsField.put("title", rset.getString("title"));
				detailsField.put("award_effective_date", rset.getTimestamp("award_effective_date"));
				detailsField.put("obligation_start", rset.getTimestamp("obligation_start"));
				detailsField.put("obligation_end", rset.getTimestamp("obligation_end"));
				detailsField.put("obligated_amount", rset.getString("obligated_amount"));
				detailsField.put("anticipated_amount", rset.getString("anticipated_amount"));
				detailsField.put(LEAD_UNIT_NUMBER, rset.getString(LEAD_UNIT_NUMBER));
				detailsField.put("lead_unit_name", rset.getString("lead_unit_name"));
				detailsField.put(SPONSOR_CODE, rset.getString(SPONSOR_CODE));
				detailsField.put(SPONSOR_NAME, commonService.getSponsorFormatBySponsorDetail(rset.getString("SPONSOR_CODE"), rset.getString("SPONSOR_NAME"), rset.getString("ACRONYM")));
				detailsField.put("award_status", rset.getString("award_status"));
				detailsField.put("last_update", rset.getString("last_update"));
				detailsField.put(ROOT_AWARD_NUMBER, rset.getString(ROOT_AWARD_NUMBER));
				detailsField.put(PERSON_ID, rset.getString(PERSON_ID));
				detailsField.put(FULL_NAME, rset.getString(FULL_NAME));
				detailsField.put(PRNCPL_NM, rset.getString(PRNCPL_NM));
				detailsField.put("is_awd_budget", rset.getString("is_awd_budget"));
				detailsField.put("latest_version_number", rset.getString("latest_version_number"));
				detailsField.put("notice_date", rset.getTimestamp("notice_date"));
				awardDetails.add(detailsField);
			}
			awardDetailsVO.setAwardDetails(awardDetails);
		} catch (SQLException e) {
			logger.error("Exception in getAwardSummaryDetails: {} ", e.getMessage());
		}
	}

	public void getPersonDetails(String awardId, AwardDetailsVO awardDetailsVO) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet rset = null;
		List<HashMap<String, Object>> awardPersons = new ArrayList<HashMap<String, Object>>();
		try {
			if (oracledb.equals("Y")) {
				String procedureName = "get_award_person_details";
				String functionCall = "{call " + procedureName + "(?,?)}";
				callstm = connection.prepareCall(functionCall);
				callstm.registerOutParameter(1, OracleTypes.CURSOR);
				callstm.setString(2, awardId);
				callstm.execute();
				rset = (ResultSet) callstm.getObject(1);
			} else if (oracledb.equalsIgnoreCase("N")) {
				callstm = connection.prepareCall("{call get_award_person_details(?)}");
				callstm.setInt(1, Integer.parseInt(awardId));
				callstm.execute();
				rset = callstm.getResultSet();
			}
			while (rset.next()) {
				HashMap<String, Object> detailsField = new HashMap<>();
				detailsField.put(AWARD_ID, rset.getString(AWARD_ID));
				detailsField.put(AWARD_NUMBER, rset.getString(AWARD_NUMBER));
				detailsField.put(PERSON_ID, rset.getString(PERSON_ID));
				detailsField.put(FULL_NAME, rset.getString(FULL_NAME));
				detailsField.put("contact_role_code", rset.getString("contact_role_code"));
				detailsField.put("unit_number", rset.getString("unit_number"));
				detailsField.put(UNITNAME, rset.getString(UNITNAME));
				detailsField.put("user_name", rset.getString(PRNCPL_NM));
				awardPersons.add(detailsField);
			}
			awardDetailsVO.setAwardPersons(awardPersons);
		} catch (SQLException e) {
			logger.error("Exception in getPersonDetails : {}", e.getMessage());
		}
	}

	public void getSponsorContactDetails(String awardId, AwardDetailsVO awardDetailsVO) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet rset = null;
		List<HashMap<String, Object>> awardSponsorContact = new ArrayList<HashMap<String, Object>>();
		try {
			if (oracledb.equals("Y")) {
				String procedureName = "get_award_spnsor_contact";
				String functionCall = "{call " + procedureName + "(?,?)}";
				callstm = connection.prepareCall(functionCall);
				callstm.registerOutParameter(1, OracleTypes.CURSOR);
				callstm.setString(2, awardId);
				callstm.execute();
				rset = (ResultSet) callstm.getObject(1);
			} else if (oracledb.equals("N")) {
				int award_id = Integer.parseInt(awardId);
				callstm = connection.prepareCall("{call get_award_spnsor_contact(?)}");
				callstm.setInt(1, award_id);
				callstm.execute();
				rset = callstm.getResultSet();
			}
			while (rset.next()) {
				HashMap<String, Object> detailsField = new HashMap<>();
				detailsField.put(AWARD_ID, rset.getString(AWARD_ID));
				detailsField.put(AWARD_NUMBER, rset.getString(AWARD_NUMBER));
				detailsField.put(FULL_NAME, rset.getString(FULL_NAME));
				detailsField.put("address_line_1", rset.getString("address_line_1"));
				detailsField.put("address_line_2", rset.getString("address_line_2"));
				detailsField.put("address_line_3", rset.getString("address_line_3"));
				detailsField.put(EMAIL_ADDRESS, rset.getString(EMAIL_ADDRESS));
				detailsField.put("city", rset.getString("city"));
				detailsField.put("postal_code", rset.getString("postal_code"));
				detailsField.put("contact_type", rset.getString("contact_type"));
				awardSponsorContact.add(detailsField);
			}
			awardDetailsVO.setAwardSponsorContact(awardSponsorContact);
		} catch (SQLException e) {
			logger.error("Exception in getSponsorContactDetails : {}", e.getMessage());
		}
	}

	public void getUnitContactDetails(String awardId, AwardDetailsVO awardDetailsVO) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet rset = null;
		List<HashMap<String, Object>> awardUnitContact = new ArrayList<HashMap<String, Object>>();
		try {
			if (oracledb.equals("Y")) {
				String procedureName = "get_award_unit_contact";
				String functionCall = "{call " + procedureName + "(?,?)}";
				callstm = connection.prepareCall(functionCall);
				callstm.registerOutParameter(1, OracleTypes.CURSOR);
				callstm.setString(2, awardId);
				callstm.execute();
				rset = (ResultSet) callstm.getObject(1);
			} else if (oracledb.equals("N")) {
				int award_id = Integer.parseInt(awardId);
				callstm = connection.prepareCall("{call get_award_unit_contact(?)}");
				callstm.setInt(1, award_id);
				callstm.execute();
				rset = callstm.getResultSet();
			}
			while (rset.next()) {
				HashMap<String, Object> detailsField = new HashMap<>();
				detailsField.put(AWARD_ID, rset.getString(AWARD_ID));
				detailsField.put("rolodex_id", rset.getString("rolodex_id"));
				detailsField.put("office_phone", rset.getString("office_phone"));
				detailsField.put(FULL_NAME, rset.getString(FULL_NAME));
				detailsField.put("email_addr", rset.getString(EMAIL_ADDRESS));
				awardUnitContact.add(detailsField);
			}
			awardDetailsVO.setAwardUnitContact(awardUnitContact);
		} catch (SQLException e) {
			logger.error("Exception in getUnitContactDetails : {}", e.getMessage());
		}
	}

	public void getFundedProposalsDetails(String awardId, AwardDetailsVO awardDetailsVO) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet rset = null;
		List<HashMap<String, Object>> awardFundedProposals = new ArrayList<HashMap<String, Object>>();
		try {
			if (oracledb.equals("Y")) {
				String procedureName = "get_award_funded_proposal";
				String functionCall = "{call " + procedureName + "(?,?)}";
				callstm = connection.prepareCall(functionCall);
				callstm.registerOutParameter(1, OracleTypes.CURSOR);
				callstm.setString(2, awardId);
				callstm.execute();
				rset = (ResultSet) callstm.getObject(1);
			} else if (oracledb.equals("N")) {
				int award_id = Integer.parseInt(awardId);
				callstm = connection.prepareCall("{call get_award_funded_proposal(?)}");
				callstm.setInt(1, award_id);
				callstm.execute();
				rset = callstm.getResultSet();
			}
			while (rset.next()) {
				HashMap<String, Object> detailsField = new HashMap<>();
				detailsField.put(AWARD_ID, rset.getString(AWARD_ID));
				detailsField.put(AWARD_NUMBER, rset.getString(AWARD_NUMBER));
				detailsField.put("proposal_number", rset.getString("proposal_number"));
				detailsField.put("pi", rset.getString("pi"));
				detailsField.put(LEAD_UNIT_NUMBER, rset.getString("home_unit_number"));
				detailsField.put(UNITNAME, rset.getString(UNITNAME));
				detailsField.put(SPONSOR_CODE, rset.getString(SPONSOR_CODE));
				detailsField.put(SPONSOR_NAME, rset.getString(SPONSOR_NAME));
				detailsField.put("start_date", rset.getString("start_date"));
				detailsField.put("end_date", rset.getString("end_date"));
				awardFundedProposals.add(detailsField);
			}
			awardDetailsVO.setAwardFundedProposals(awardFundedProposals);
		} catch (SQLException e) {
			logger.error("Exception in getFundedProposalsDetails : {}", e.getMessage());
		}
	}

	public void getSpecialReviewsDetails(String awardId, AwardDetailsVO awardDetailsVO) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet rset = null;
		try {
			if (oracledb.equals("Y")) {
				String procedureName = "get_award_special_review";
				String functionCall = "{call " + procedureName + "(?,?)}";
				callstm = connection.prepareCall(functionCall);
				callstm.registerOutParameter(1, OracleTypes.CURSOR);
				callstm.setString(2, awardId);
				callstm.execute();
				rset = (ResultSet) callstm.getObject(1);
			} else if (oracledb.equals("N")) {
				int award_id = Integer.parseInt(awardId);
				callstm = connection.prepareCall("{call get_award_special_review(?)}");
				callstm.setInt(1, award_id);
				callstm.execute();
				rset = callstm.getResultSet();
			}
			List<HashMap<String, Object>> awardSpecialReviews = new ArrayList<>();
			while (rset.next()) {
				HashMap<String, Object> detailsField = new HashMap<>();
				detailsField.put(AWARD_ID, rset.getString(AWARD_ID));
				detailsField.put("special_review_code", rset.getString("special_review_code"));
				detailsField.put("special_review_type", rset.getString("special_review_type"));
				detailsField.put("approval_type_code", rset.getString("approval_type_code"));
				detailsField.put(DESCRIPTION, rset.getString(DESCRIPTION));
				detailsField.put("approval_status", rset.getString("approval_status"));
				detailsField.put("protocol_number", rset.getString("protocol_number"));
				detailsField.put("application_date", rset.getString("application_date"));
				detailsField.put("approval_date", rset.getString("approval_date"));
				detailsField.put("expiration_date", rset.getString("expiration_date"));
				awardSpecialReviews.add(detailsField);
			}
			awardDetailsVO.setAwardSpecialReviews(awardSpecialReviews);
		} catch (SQLException e) {
			logger.error("Exception in getSpecialReviewsDetails : {}", e.getMessage());
		}
	}

	public void getHierarchyDetails(String awardNumber, AwardHierarchyVO awardHierarchyVO, String selectedAwardNumber) {
		AwardHierarchyDto returnAwardHierarchy = new AwardHierarchyDto();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection con = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet rset = null;
		HashMap<String, AwardHierarchyDto> hmAwards = new HashMap<>();
		AwardHierarchyDto parentHierarchy = new AwardHierarchyDto();
		try {
			if (oracledb.equalsIgnoreCase("Y")) {
				String aProcedureName = "get_award_hierarchy";
				String functionCall = "{call " + aProcedureName + "(?,?)}";
				callstm = con.prepareCall(functionCall);
				callstm.registerOutParameter(1, OracleTypes.CURSOR);
				callstm.setString(2, selectedAwardNumber);
				callstm.execute();
				rset = (ResultSet) callstm.getObject(1);
			} else if (oracledb.equalsIgnoreCase("N")) {
				callstm = con.prepareCall("{call get_award_hierarchy(?)}");
				callstm.setString(1, selectedAwardNumber);
				callstm.execute();
				rset = callstm.getResultSet();
			}
			String parentAwardNumber = "";
			boolean isFirstTime = true;
			while (rset.next()) {
				AwardHierarchyDto awardHierarchy = new AwardHierarchyDto();
				awardHierarchy.setAwardNumber((String) rset.getString(AWARD_NUMBER));
				if (isFirstTime) {
					parentAwardNumber = awardHierarchy.getAwardNumber();
					isFirstTime = false;
				}
				awardHierarchy.setSelected(selectedAwardNumber.equalsIgnoreCase((String) rset.getString(AWARD_NUMBER)) ? true : false);
				awardHierarchy.setOpen(true);
				awardHierarchy.setParentAwardNumber((String) rset.getString(PARENT_AWARD_NUMBER));
				awardHierarchy.setPrincipalInvestigator((String) rset.getString(PI_NAME));
				awardHierarchy.setAccountNumber((String) rset.getString(ACCOUNT_NUMBER));
				awardHierarchy.setStatusCode(Integer.parseInt((rset.getString(STATUSCODE))));
				awardHierarchy.setAwardStatus(fetchAwardStatusByCode((rset.getString(STATUSCODE))));
				awardHierarchy.setAwardId(rset.getString(AWARD_ID));
				awardHierarchy.setRootAwardNumber(rset.getString(ROOT_AWARD_NUMBER));
				awardHierarchy.setAwardDocumentTypeCode(rset.getInt("AWARD_DOCUMENT_TYPE_CODE"));
				awardHierarchy.setAwardSequenceStatus(rset.getString("AWARD_SEQUENCE_STATUS"));
				awardHierarchy.setWorkflowAwardStatusCode(rset.getInt("WORKFLOW_AWARD_STATUS_CODE"));
				String accountNumber;
				String piName;
				if ((String) rset.getString(ACCOUNT_NUMBER) == null) {
					accountNumber = "";
				} else {
					accountNumber = " : " + rset.getString(ACCOUNT_NUMBER);
				}
				if ((String) rset.getString(PI_NAME) == null) {
					piName = "";
				} else {
					piName = " : " + rset.getString(PI_NAME);
				}
				awardHierarchy.setName(rset.getString(AWARD_NUMBER) + accountNumber + piName);
				awardHierarchy.setLevel(Integer.valueOf(rset.getString("level")));

				if (!hmAwards.isEmpty()) {
					parentHierarchy = hmAwards.get((String) rset.getString(PARENT_AWARD_NUMBER));
					if (parentHierarchy != null) {
						List<AwardHierarchyDto> awardHierarchyDtos = parentHierarchy.getChildren();
						if (awardHierarchyDtos == null) {
							awardHierarchyDtos = new ArrayList<>();
						}
						awardHierarchyDtos.add(awardHierarchy);
						parentHierarchy.setChildren(awardHierarchyDtos);
					}
				}
				hmAwards.put((String) rset.getString(AWARD_NUMBER), awardHierarchy);
			}
			returnAwardHierarchy = hmAwards.get(parentAwardNumber);
			awardHierarchyVO.setAwardHierarchy(returnAwardHierarchy);
		} catch (SQLException e) {
			logger.error(	"Exception in getHierarchyDetails : {}", e.getMessage());
		}
	}

	public void getBudgetVersionDetails(String awardNumber, AwardHierarchyVO hierarchyVO) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection con = sessionImpl.connection();
		CallableStatement callstm = null;
		String aProcedureName = "get_fibi_award_budget_versions";
		String functionCall = "{call " + aProcedureName + "(?,?)}";
		try {
			callstm = con.prepareCall(functionCall);
			callstm.registerOutParameter(1, OracleTypes.CURSOR);
			callstm.setString(2, awardNumber);
			callstm.execute();

			ResultSet rset = (ResultSet) callstm.getObject(1);
			List<HashMap<String, Object>> hierarchyBudgetVersions = new ArrayList<HashMap<String, Object>>();
			while (rset.next()) {
				HashMap<String, Object> detailsField = new HashMap<>();
				detailsField.put(AWARD_ID, rset.getString(AWARD_ID));
				detailsField.put(AWARD_NUMBER, rset.getString(AWARD_NUMBER));
				detailsField.put("version_number", rset.getString("version_number"));
				detailsField.put("budget_start", rset.getString("budget_start"));
				detailsField.put("budget_end", rset.getString("budget_end"));
				detailsField.put("award_budget_type_code", rset.getString("award_budget_type_code"));
				detailsField.put("budget_type", rset.getString("budget_type"));
				detailsField.put("on_off_campus", rset.getString("on_off_campus"));
				detailsField.put("direct_cost", rset.getString("direct_cost"));
				detailsField.put("total_indirect_cos", rset.getString("total_indirect_cos"));
				detailsField.put("total_cost", rset.getString("total_cost"));
				detailsField.put("fa_rate_type", rset.getString("fa_rate_type"));
				detailsField.put("last_updated", rset.getString("last_updated"));
				hierarchyBudgetVersions.add(detailsField);
			}
			hierarchyVO.setHierarchyBudgetVersions(hierarchyBudgetVersions);
		} catch (SQLException e) {
			logger.error("Exception in getBudgetVersionDetails : {}", e.getMessage());
		}
	}

	@Override
	public CommonVO getDropDownDatas(CommonVO vo) {
		getCategoryList(vo);
		getTypeList(vo);
		getDepartmentList(vo);
		logger.info("CommonVO : {}", vo);
		return vo;
	}

	public void getCategoryList(CommonVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<HashMap<String, Object>> categorys = new ArrayList<>();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call get_ost_category()}");
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "get_ost_category";
				String functionCall = "{call " + procedureName + "(?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset.next()) {
				HashMap<String, Object> categoryMap = new HashMap<>();
				categoryMap.put(CATEGORY_CODE, rset.getString(CATEGORY_CODE));
				categoryMap.put(DESC, rset.getString(DESC));
				categorys.add(categoryMap);
			}
			vo.setCategoryMap(categorys);
		} catch (SQLException e) {
			logger.error("Exception in getCategoryList : {}", e.getMessage());
		}
	}

	public void getTypeList(CommonVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<HashMap<String, Object>> types = new ArrayList<>();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call get_ost_type()}");
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "get_ost_type";
				String functionCall = "{call " + procedureName + "(?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset.next()) {
				HashMap<String, Object> typeMap = new HashMap<>();
				typeMap.put(CATEGORY_CODE, rset.getString(CATEGORY_CODE));
				typeMap.put(DESC, rset.getString(DESC));
				typeMap.put("TOOL_TIP", rset.getString("TOOL_TIP"));
				typeMap.put("TYPE_CODE", rset.getString("TYPE_CODE"));
				typeMap.put("HELP_LINK", rset.getString("HELP_LINK"));
				types.add(typeMap);
			}
			vo.setTypeMap(types);
		} catch (SQLException e) {
			logger.error("Exception in getTypeList : {}", e.getMessage());
		}
	}

	public void getDepartmentList(CommonVO vo) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<HashMap<String, Object>> departments = new ArrayList<>();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (vo.getIsUnitAdmin() == true) {
				if (oracledb.equalsIgnoreCase("N")) {
					statement = connection.prepareCall("{call GET_OST_ALL_UNITS()}");
					statement.execute();
					rset = statement.getResultSet();
				} else if (oracledb.equalsIgnoreCase("Y")) {
					String procedureName = "GET_OST_ALL_UNITS";
					String functionCall = "{call " + procedureName + "(?)}";
					statement = connection.prepareCall(functionCall);
					statement.registerOutParameter(1, OracleTypes.CURSOR);
					statement.execute();
					rset = (ResultSet) statement.getObject(1);
				}
				while (rset.next()) {
					HashMap<String, Object> departmentMap = new HashMap<>();
					departmentMap.put("AO_FULL_NAME", rset.getString("AO_FULL_NAME"));
					departmentMap.put("AO_PERSON_ID", rset.getString("AO_PERSON_ID"));
					departmentMap.put(UNIT_NAME, rset.getString(UNIT_NAME));
					departmentMap.put(UNIT_NUMBER, rset.getString(UNIT_NUMBER));
					departments.add(departmentMap);
				}
			} else {
				if (oracledb.equalsIgnoreCase("N")) {
					statement = connection.prepareCall("{call get_ost_person_units(?)}");
					statement.setString(1, vo.getPersonId());
					statement.execute();
					rset = statement.getResultSet();
				} else if (oracledb.equalsIgnoreCase("Y")) {
					String procedureName = "get_ost_person_units";
					String functionCall = "{call " + procedureName + "(?,?)}";
					statement = connection.prepareCall(functionCall);
					statement.setString(1, vo.getPersonId());
					statement.registerOutParameter(2, OracleTypes.CURSOR);
					statement.execute();
					rset = (ResultSet) statement.getObject(2);
				}
				while (rset.next()) {
					HashMap<String, Object> departmentMap = new HashMap<>();
					departmentMap.put("LIST_ORDER", rset.getString("LIST_ORDER"));
					departmentMap.put("OSP_ADMINISTRATOR", rset.getString("OSP_ADMINISTRATOR"));
					departmentMap.put("OSP_PERSON_ID", rset.getString("OSP_PERSON_ID"));
					departmentMap.put(UNIT_NAME, rset.getString(UNIT_NAME));
					departmentMap.put(UNIT_NUMBER, rset.getString(UNIT_NUMBER));
					departments.add(departmentMap);
				}
			}
			vo.setDepartmentList(departments);
		} catch (SQLException e) {
			logger.error("Exception in getDepartmentList : {}", e.getMessage());
		}
	}

	@Lock(LockModeType.PESSIMISTIC_WRITE)
	@Override
	public Award saveOrUpdateAwardDetails(Award award) {
		hibernateTemplate.saveOrUpdate(award);
		return award;
	}

	@Override
	public List<AwardType> fetchAllAwardTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardType> query = builder.createQuery(AwardType.class);
		Root<AwardType> rootUnit = query.from(AwardType.class);
		query.orderBy(builder.asc(rootUnit.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardStatus> fetchAllAwardStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardStatus> query = builder.createQuery(AwardStatus.class);
		Root<AwardStatus> rootAgreementSponsorType = query.from(AwardStatus.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Award getAwardDetailsById(Integer awardId) {
		return hibernateTemplate.get(Award.class, awardId);
	}

	@Override
	public AwardKeyword saveOrUpdateAwardKeyword(AwardKeyword awardKeyword) {
		hibernateTemplate.saveOrUpdate(awardKeyword);
		return awardKeyword;
	}

	@Override
	public AwardSpecialReview saveOrUpdateAwardSpecialReview(AwardSpecialReview specialReview) {
		try {
			hibernateTemplate.saveOrUpdate(specialReview);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardSpecialReview : {}", e.getMessage());
			throw new ApplicationException("Error in saveOrUpdateAwardSpecialReview", e, Constants.JAVA_ERROR);
		}
		return specialReview;
	}

	@Override
	public List<AwardSpecialReview> getAwardSpecialReviewsByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardSpecialReview> query = builder.createQuery(AwardSpecialReview.class);
		Root<AwardSpecialReview> awardSpecialReview = query.from(AwardSpecialReview.class);
		query.where(builder.equal(awardSpecialReview.get(AWARDID), awardId));
		query.orderBy(builder.asc(awardSpecialReview.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardSubContract> getSubContractsByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardSubContract> query = builder.createQuery(AwardSubContract.class);
		Root<AwardSubContract> subContract = query.from(AwardSubContract.class);
		query.where(builder.equal(subContract.get(AWARDID), awardId));
		query.orderBy(builder.asc(subContract.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardCostShare saveOrUpdateAwardCostShare(AwardCostShare awardCostShare) {
		try {
			hibernateTemplate.saveOrUpdate(awardCostShare);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardCostShare : {}", e.getMessage());
			throw new ApplicationException("Error in saveOrUpdateAwardCostShare", e, Constants.JAVA_ERROR);
		}
		return awardCostShare;
	}

	@Override
	public AwardSubContract saveOrUpdateAwardSubContract(AwardSubContract subContract) {
		try {
			hibernateTemplate.saveOrUpdate(subContract);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardSubContract : {}", e.getMessage());
			throw new ApplicationException("Error occurred in saveOrUpdateAwardSubContract", e, Constants.JAVA_ERROR);
		}
		return subContract;
	}

	@Override
	public void deleteAwardSpecialReview(Integer awardSpecailReviewId) {
		if (awardSpecailReviewId != null) {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardSpecialReview> query = builder.createQuery(AwardSpecialReview.class);
			Root<AwardSpecialReview> root = query.from(AwardSpecialReview.class);
			query.where(builder.equal(root.get("awardSpecailReviewId"), awardSpecailReviewId));
			hibernateTemplate.delete(session.createQuery(query).uniqueResult());
		}
	}

	@Override
	public void deleteAwardSubContract(Integer awardSubawardId) {
		if (awardSubawardId != null) {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardSubContract> query = builder.createQuery(AwardSubContract.class);
			Root<AwardSubContract> root = query.from(AwardSubContract.class);
			query.where(builder.equal(root.get("awardApprovedSubawardId"), awardSubawardId));
			hibernateTemplate.delete(session.createQuery(query).uniqueResult());
		}
	}

	@Override
	public void deleteAwardCostShare(Integer awardCostShareId) {
		if (awardCostShareId != null) {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardCostShare> query = builder.createQuery(AwardCostShare.class);
			Root<AwardCostShare> root = query.from(AwardCostShare.class);
			query.where(builder.equal(root.get("awardCostShareId"), awardCostShareId));
			hibernateTemplate.delete(session.createQuery(query).uniqueResult());
		}
	}

	@Override
	public List<ContactRoleType> getPersonnelRoleList() {
		return hibernateTemplate.loadAll(ContactRoleType.class);
	}

	@Override
	public AwardPerson saveOrUpdateAwardPersonDetails(AwardPerson awardPerson) {
		try {
			hibernateTemplate.saveOrUpdate(awardPerson);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardPersonDetails : {}", e.getMessage());
		}
		return awardPerson;
	}

	@Override
	public AwardContact saveOrUpdateAwardContactDetails(AwardContact awardContact) {
		try {
			hibernateTemplate.saveOrUpdate(awardContact);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardContactDetails : {}", e.getMessage());
		}
		return awardContact;
	}

	@Override
	public AwardPerson saveAwardKeyPersonnel(AwardPerson awardPerson) {
		hibernateTemplate.saveOrUpdate(awardPerson);
		return awardPerson;
	}

	@Override
	public void deleteAwardPersonnel(Integer awardPersonId) {
		hibernateTemplate.delete(hibernateTemplate.get(AwardPerson.class, awardPersonId));
	}

	@Override
	public AwardProjectTeam saveOrUpdateAwardProjectTeam(AwardProjectTeam awardProjectTeam) {
			try {
				hibernateTemplate.saveOrUpdate(awardProjectTeam);
			} catch (Exception e) {
				logger.error("Exception in saveOrUpdateAwardProjectTeam : {}", e.getMessage());
				throw new ApplicationException("Error in saveOrUpdateAwardProjectTeam", e, Constants.JAVA_ERROR);
			}
		return awardProjectTeam;
	}

	@Override
	public List<AwardContactType> getAwardContactTypeList() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardContactType> query = builder.createQuery(AwardContactType.class);
		Root<AwardContactType> rootAgreementSponsorType = query.from(AwardContactType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardPerson> getAwardPersonList(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().openSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardPerson> query = builder.createQuery(AwardPerson.class);
		Root<AwardPerson> rootAwardPerson = query.from(AwardPerson.class);
		query.where(builder.equal(rootAwardPerson.get(AWARDID), awardId));
		query.orderBy(builder.asc(rootAwardPerson.get("proposalPersonRole").get("sortId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardPerson> getAwardPersonListForTerms(Integer awardId) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT NEW com.polus.fibicomp.award.pojo.AwardPerson( T1.awardPersonId, T1.awardId, T1.awardNumber, T1.sequenceNumber, T1.personId, T1.fullName, T1.rolodexId ) ");
		hqlQuery.append("from AwardPerson T1 inner join ProposalPersonRole T2 on T1.personRoleId = T2.id  where  T1.awardId = : awardId order by T2.sortId asc");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("awardId", awardId);
		return query.getResultList();
	}

	@Override
	public List<AwardContact> getAwardContactList(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardContact> query = builder.createQuery(AwardContact.class);
		Root<AwardContact> rootAwardContact = query.from(AwardContact.class);
		query.where(builder.equal(rootAwardContact.get(AWARDID), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardProjectTeam> getAwardProjectTeamList(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardProjectTeam> query = builder.createQuery(AwardProjectTeam.class);
		Root<AwardProjectTeam> rootAwardProjectTeam = query.from(AwardProjectTeam.class);
		query.where(builder.equal(rootAwardProjectTeam.get(AWARDID), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void saveAwardPersonUnits(AwardPersonUnit awardPersonUnit) {
		if (awardPersonUnit != null) {
			try {
				hibernateTemplate.saveOrUpdate(awardPersonUnit);
			} catch (Exception e) {
				logger.error("Exception in saveAwardPersonUnits : {}", e.getMessage());
			}
		}
	}

	@Override
	public String getAwardNameByUnitNumber(String unitNumber) {
		String unitName = "";
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<Unit> rootUnit = query.from(Unit.class);
		query.where(builder.equal(rootUnit.get("unitNumber"), unitNumber));
		query.multiselect(builder.max(rootUnit.get("unitName")));
		if (session.createQuery(query).uniqueResult() != null) {
			unitName = session.createQuery(query).uniqueResult();
			logger.info("projections value : {}", unitName);
		}
		return unitName;
	}

	@Override
	public String deleteAwardReportRecepientById(AwardReportTermRecipient awardReportTermRecipient) {
		try {
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"delete from AwardReportTermRecipient d where d.awardReportTerms.awardReportTermsId = :awardReportTermsId AND d.awardId = :awardId AND d.recipientId = :recipientId");
			query.setParameter(AWARD_REPORT_TERMS_ID, awardReportTermRecipient.getAwardReportTerms().getAwardReportTermsId());
			query.setParameter(AWARDID, awardReportTermRecipient.getAwardId());
			query.setParameter("recipientId", awardReportTermRecipient.getRecipientId());
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("Exception in deleteAwardReportRecepientById : {}", e.getMessage());
		}
		return "Award Report recepient deleted successfully";
	}

	@Override
	public String deleteAwardPersonnelUnitByUnitId(AwardPersonUnit awardPersonUnit) {
		try {
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"delete from AwardPersonUnit d where d.awardPersonId = :awardPersonId AND d.awardId = :awardId AND d.unitNumber = :unitNumber");
			query.setParameter("awardPersonId", awardPersonUnit.getAwardPerson().getAwardPersonId());
			query.setParameter(AWARDID, awardPersonUnit.getAwardId());
			query.setParameter("unitNumber", awardPersonUnit.getUnitNumber());
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("Exception in deleteAwardPersonnelUnitByUnitId : {}", e.getMessage());
		}
		return "Award Unit deleted successfully";
	}

	@Override
	public List<ReportClass> getReportClassList() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ReportClass> query = builder.createQuery(ReportClass.class);
		Root<ReportClass> rootAgreementSponsorType = query.from(ReportClass.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Report> getReportList() {
		return hibernateTemplate.loadAll(Report.class);
	}

	@Override
	public List<Frequency> getFrequencyList() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builderFrequency = session.getCriteriaBuilder();
		CriteriaQuery<Frequency> queryFrequency = builderFrequency.createQuery(Frequency.class);
		Root<Frequency> rootFrequency = queryFrequency.from(Frequency.class);
		queryFrequency.orderBy(builderFrequency.asc(rootFrequency.get("sortOrder")));
		queryFrequency.where(builderFrequency.equal(rootFrequency.get("isInvoiceFrequency"),"N" ));
		return session.createQuery(queryFrequency).getResultList();
	}

	@Override
	public List<Frequency> getInvoiceFrequencyList() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builderFrequency = session.getCriteriaBuilder();
		CriteriaQuery<Frequency> queryFrequency = builderFrequency.createQuery(Frequency.class);
		Root<Frequency> rootFrequency = queryFrequency.from(Frequency.class);
		queryFrequency.orderBy(builderFrequency.asc(rootFrequency.get("sortOrder")));
		queryFrequency.where(builderFrequency.equal(rootFrequency.get("isInvoiceFrequency"),"Y" ));
		return session.createQuery(queryFrequency).getResultList();
	}

	@Override
	public List<FrequencyBase> getFrequencyBaseList() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builderFrequencyBase = session.getCriteriaBuilder();
		CriteriaQuery<FrequencyBase> queryFrequencyBase = builderFrequencyBase.createQuery(FrequencyBase.class);
		Root<FrequencyBase> rootFrequencyBase = queryFrequencyBase.from(FrequencyBase.class);
		queryFrequencyBase.orderBy(builderFrequencyBase.asc(rootFrequencyBase.get("sortOrder")));
		return session.createQuery(queryFrequencyBase).getResultList();
	}

	@Override
	public List<Distribution> getDistributionList() {
		return hibernateTemplate.loadAll(Distribution.class);
	}

	@Override
	public List<SponsorTermType> getSponsorTermTypeList() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorTermType> query = builder.createQuery(SponsorTermType.class);
		Root<SponsorTermType> rootAgreementSponsorType = query.from(SponsorTermType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String deleteAwardSponsorTerm(AwardSponsorTerm awardSponsorTerm) {
		hibernateTemplate.delete(awardSponsorTerm);
		String response = "Award Sponsor Term deleted successfully";
		return commonDao.convertObjectToJSON(response);
	}

	@Override
	public List<SponsorTerm> getSponsorTermList() {
		return hibernateTemplate.loadAll(SponsorTerm.class);
	}

	@Override
	public AwardSponsorTerm saveOrUpdateAwardSponsorTerms(AwardSponsorTerm awardSponsorTerm) {
		hibernateTemplate.saveOrUpdate(awardSponsorTerm);
		return awardSponsorTerm;
	}

	@Override
	public AwardReportTerms saveOrUpdateAwardReports(AwardReportTerms awardReport) {
		hibernateTemplate.saveOrUpdate(awardReport);
		return awardReport;
	}

	@Override
	public String deleteAwardReport(Integer awardReportTermsId) {
		hibernateTemplate.delete(hibernateTemplate.get(AwardReportTerms.class, awardReportTermsId));
		String response = "Award Report deleted successfully";
		return commonDao.convertObjectToJSON(response);
	}

	@Override
	public AwardApprovedEquipment saveOrUpdateAwardApprovedEquipment(AwardApprovedEquipment awardApprovedEquipment) {
		try {
			hibernateTemplate.saveOrUpdate(awardApprovedEquipment);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardApprovedEquipment : {}", e.getMessage());
			throw new ApplicationException("Error occurred in saveOrUpdateAwardApprovedEquipment", e, Constants.JAVA_ERROR);
		}
		return awardApprovedEquipment;
	}

	@Override
	public AwardAprovedForeignTravel saveOrUpdateAwardAprovedForeignTravel(AwardAprovedForeignTravel awardAprovedForeignTravel) {
		try {
			hibernateTemplate.saveOrUpdate(awardAprovedForeignTravel);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardAprovedForeignTravel : {}", e.getMessage());
			throw new ApplicationException("Error occurred in saveOrUpdateAwardAprovedForeignTravel", e, Constants.JAVA_ERROR);
		}
		return awardAprovedForeignTravel;
	}

	@Override
	public void deleteAwardApprovedEquipment(AwardApprovedEquipment awardApprovedEquipment) {
		if (awardApprovedEquipment != null) {
			try {
				hibernateTemplate.delete(awardApprovedEquipment);
			} catch (Exception e) {
				logger.error("Exception in deleteAwardApprovedEquipment : {}", e.getMessage());
			}
		}
	}

	@Override
	public void deleteAwardAprovedForeignTravel(AwardAprovedForeignTravel awardAprovedForeignTravel) {
		if (awardAprovedForeignTravel != null) {
			try {
				hibernateTemplate.delete(awardAprovedForeignTravel);
			} catch (Exception e) {
				logger.error("Exception in deleteAwardAprovedForeignTravel : {}", e.getMessage());
			}
		}
	}

	@Override
	public SponsorTerm getSponsorTermByCode(String sponsorTermCode, String sponsorTermTypeCode) {
		SponsorTerm sponsorTerm = new SponsorTerm();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorTerm> query = builder.createQuery(SponsorTerm.class);
		Root<SponsorTerm> rootSponsorTerm = query.from(SponsorTerm.class);
		Predicate predicate1 = builder.equal(rootSponsorTerm.get("sponsorTermCode"), sponsorTermCode);
		Predicate predicate2 = builder.equal(rootSponsorTerm.get(SPONSOR_TERM_TYPECODE), sponsorTermTypeCode);
		query.where(builder.and(predicate1, predicate2));
		if (session.createQuery(query).getResultList() != null) {
			sponsorTerm = session.createQuery(query).getSingleResult();
		}
		return sponsorTerm;
	}

	public SponsorTermType getSponsorTermTypeByCode(String sponsorTermTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorTermType> query = builder.createQuery(SponsorTermType.class);
		Root<SponsorTermType> rootSponsorTermType = query.from(SponsorTermType.class);
		query.where(builder.equal(rootSponsorTermType.get(SPONSOR_TERM_TYPECODE), sponsorTermTypeCode));
		return session.createQuery(query).getSingleResult();
	}

	public void getAwardSponsorTerms(Integer awardId, ReportTermsVO reportTermsVO) {
		List<AwardSponsorTerm> awardSponsorTermList = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardSponsorTerm> query = builder.createQuery(AwardSponsorTerm.class);
		Root<AwardSponsorTerm> rootAwardSponsorTerm = query.from(AwardSponsorTerm.class);
		query.where(builder.equal(rootAwardSponsorTerm.get(AWARDID), awardId));
		try {
			if (session.createQuery(query).getResultList() != null) {
				awardSponsorTermList = session.createQuery(query).getResultList();
			}

			Map<String, List<HashMap<String, Object>>> awardTermsMapList = new HashMap<>();
			for (AwardSponsorTerm awardSponsorTerm : awardSponsorTermList) {
				HashMap<String, Object> sponsorTermDetailsField = new HashMap<>();
				sponsorTermDetailsField.put(AWARDID, awardSponsorTerm.getAwardId());
				sponsorTermDetailsField.put(AWARDNUMBER, awardSponsorTerm.getAwardNumber());
				sponsorTermDetailsField.put(SPONSOR_TERM_TYPECODE, awardSponsorTerm.getSponsorTermTypeCode());
				sponsorTermDetailsField.put("awardSponsorTermId", awardSponsorTerm.getAwardSponsorTermId());
				SponsorTermType sponsorTermType = getSponsorTermTypeByCode(awardSponsorTerm.getSponsorTermTypeCode());
				sponsorTermDetailsField.put("sponsorTermType", sponsorTermType.getDescription());
				sponsorTermDetailsField.put("sponsorTermCode", awardSponsorTerm.getSponsorTermCode());
				SponsorTerm sponsorTerm = getSponsorTermByCode(awardSponsorTerm.getSponsorTermCode(), awardSponsorTerm.getSponsorTermTypeCode());
				sponsorTermDetailsField.put("sponsorTerm", sponsorTerm.getDescription());
				sponsorTermDetailsField.put("acType", "U");

				List<HashMap<String, Object>> sponsorTermsListByType = new ArrayList<>();
				sponsorTermsListByType.add(sponsorTermDetailsField);

				if (!awardTermsMapList.containsKey(sponsorTermType.getDescription())) {
					awardTermsMapList.put(sponsorTermType.getDescription(), sponsorTermsListByType);
				} else {
					awardTermsMapList.get(sponsorTermType.getDescription()).add(sponsorTermDetailsField);
				}
			}
			reportTermsVO.setAwardTermsList(awardTermsMapList);
		} catch (Exception e) {
			logger.error("Exception in getAwardSponsorTerms : {}", e.getMessage());
		}
	}

	@Override
	public ReportClass getReportClassByCode(String reportClassCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ReportClass> query = builder.createQuery(ReportClass.class);
		Root<ReportClass> rootReportClass = query.from(ReportClass.class);
		query.where(builder.equal(rootReportClass.get(REPORTCLASSCODE), reportClassCode));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public void getAwardSponsorReports(Integer awardId, ReportTermsVO reportTermsVO) {
		List<AwardReportTerms> awardReportTerms = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardReportTerms> query = builder.createQuery(AwardReportTerms.class);
		Root<AwardReportTerms> rootAwardSponsorTerm = query.from(AwardReportTerms.class);
		query.where(builder.equal(rootAwardSponsorTerm.get(AWARDID), awardId));
		try {		
			awardReportTerms = session.createQuery(query).getResultList();
			awardReportTerms.forEach(awardReportTerm -> {																	  																																							
				for (AwardReportTermRecipient awardReportTermRecipient : awardReportTerm.getAwardReportTermRecipient()) {
					awardReportTermRecipient.setFullName(getAwardReportRecipientById(awardReportTermRecipient.getRecipientId()));
				}							  
				awardReportTerm.setReportClass(getReportClassByCode(awardReportTerm.getReportClassCode()));
				awardReportTerm.setReportName(getReportNameByReportCode(awardReportTerm.getReportCode()));
			});
			reportTermsVO.setAwardReportTerms(awardReportTerms);
		} catch (Exception e) {
			logger.error("Exception in getAwardSponsorReports : {}", e.getMessage());
		}
	}

	@Override
	public AwardFundingProposal saveOrUpdateFundingProposal(AwardFundingProposal awardFundingProposal) {
		try {
			hibernateTemplate.saveOrUpdate(awardFundingProposal);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateFundingProposal : {}", e.getMessage());
			throw new ApplicationException("Error occurred in saveOrUpdateFundingProposal", e, Constants.JAVA_ERROR);
		}
		return awardFundingProposal;
	}

	@Override
	public void deleteFundingProposal(Integer awardFundingProposalId) {
		hibernateTemplate.delete(hibernateTemplate.get(AwardFundingProposal.class, awardFundingProposalId));
	}

	@Override
	public Boolean checkIpLinkedInAwards(Integer proposalId) {
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
				"select count(*) from AwardFundingProposal where proposalId =:proposalId");
		query.setParameter("proposalId",proposalId);
		if ((Long) query.getSingleResult() > 0) {
			return true;
		}
		return false;
	}

	@Override
	public String getAwardReportRecipientById(String recipientId) {
		String fullName = "";
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<String> query = builder.createQuery(String.class);
			Root<Person> rootPerson = query.from(Person.class);
			query.where(builder.equal(rootPerson.get("personId"), recipientId));
			query.multiselect(builder.max(rootPerson.get("fullName")));
			fullName = session.createQuery(query).uniqueResult();
		} catch (Exception e) {
			logger.error("Exception in getAwardReportRecipientById : {}", e.getMessage());
		}
		return fullName;
	}

	@Override
	public List<AwardFundingProposal> getAwardFundingProposals(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardFundingProposal> query = builder.createQuery(AwardFundingProposal.class);
		Root<AwardFundingProposal> rootAwardFundingProposals = query.from(AwardFundingProposal.class);
		query.where(builder.equal(rootAwardFundingProposals.get(AWARDID), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer updateAwardNextValue() {
		Query update = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery("update AWARD_NEXTVALUE set AWARD_NUMBER = AWARD_NUMBER+1");
		update.executeUpdate();
		Query select = hibernateTemplate.getSessionFactory().getCurrentSession().createSQLQuery("select MAX(AWARD_NUMBER) from AWARD_NEXTVALUE");
		return (Integer) select.getSingleResult();
	}

	@Override
	public InstituteProposal fetchInstProposalById(Integer proposalId) {
		return hibernateTemplate.get(InstituteProposal.class, proposalId);
	}

	@Override
	public Integer fetchDevProposalByInstProposal(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<InstituteProposalAdminDetail> query = builder.createQuery(InstituteProposalAdminDetail.class);
		Root<InstituteProposalAdminDetail> proposalAdminDetail = query.from(InstituteProposalAdminDetail.class);
		query.where(builder.equal(proposalAdminDetail.get("instProposalId"), proposalId));
		query.orderBy(builder.desc(proposalAdminDetail.get("proposalAdminDetailId")));
		InstituteProposalAdminDetail instPropAdminDetails = session.createQuery(query).setFirstResult(0).setMaxResults(1).getSingleResult();
		if (instPropAdminDetails != null) {
			return instPropAdminDetails.getDevProposalId();
		} else {
			return null;
		}
	}

	public void feedAwardBudgetFromIP(Integer proposalId, Integer awardId, String updateUser) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet resultSet = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				callstm = connection.prepareCall("{call FEED_AWARD_BUDGET(?,?,?)}");
				callstm.setInt(1, proposalId);
				callstm.setDouble(2, awardId);
				callstm.setString(3, updateUser);
				callstm.execute();
				resultSet = callstm.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "FEED_AWARD_BUDGET";
				String functionCall = "{call " + procedureName + "(?,?,?)}";
				callstm = connection.prepareCall(functionCall);
				callstm.setInt(1, proposalId);
				callstm.setDouble(2, awardId);
				callstm.setString(3, updateUser);
				callstm.execute();
				resultSet = (ResultSet) callstm.getObject(1);
			}
		} catch (Exception e) {
			logger.error("Exception in feedAwardBudgetFromIP : {}", e.getMessage());
		}
	}

	@Override
	public void deleteAwardAttachment(Integer awardAttachmentId) {
		if (awardAttachmentId != null) {
			try {
				hibernateTemplate.delete(hibernateTemplate.get(AwardAttachment.class, awardAttachmentId));
			} catch (Exception e) {
				logger.error("Exception in deleteAwardAttachment : {}", e.getMessage());
			}
		}
	}

	@Override
	public AwardAttachment saveAttachment(AwardAttachment awardAttachment) {
		try {
			hibernateTemplate.saveOrUpdate(awardAttachment);
		} catch (Exception e) {
			logger.error("Exception in saveAttachment : {}", e.getMessage());
		}
		return awardAttachment;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardReportTracking> getReportTrackingDetails(Integer awardId, Integer awardReportTermsId) {
		Criteria criteria = hibernateTemplate.getSessionFactory().getCurrentSession().createCriteria(AwardReportTracking.class);
		criteria.add(Restrictions.eq(AWARDID, awardId));
		criteria.add(Restrictions.eq("awardReportTerms.awardReportTermsId", awardReportTermsId));
		return criteria.list();
	}

	@Override
	public List<ReportStatus> getReportStatusList() {
		return hibernateTemplate.loadAll(ReportStatus.class);
	}

	@Override
	public AwardReportTerms getAwardReportTermsById(Integer awardReportTermsId) {
		return hibernateTemplate.get(AwardReportTerms.class, awardReportTermsId);
	}

	@Override
	public Frequency getFrequencyByFrequencyCode(String frequencyCode) {
		return hibernateTemplate.get(Frequency.class, frequencyCode);
	}

	@Override
	public ReportStatus getPendingReportStatus(String statusCode) {
		return hibernateTemplate.get(ReportStatus.class, statusCode);
	}

	@Override
	public void deleteAwardReportTrackingById(AwardReportTerms awardReportTerms, List<Integer> awardReportTrackingIds) {
		try {
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"delete from AwardReportTracking d where d.awardReportTerms.awardReportTermsId = :awardReportTermsId AND d.awardId = :awardId AND d.awardReportTrackingId in( :awardReportTrackingId)");
			query.setParameter(AWARD_REPORT_TERMS_ID, awardReportTerms.getAwardReportTermsId());
			query.setParameter(AWARDID, awardReportTerms.getAwardId());
			query.setParameter("awardReportTrackingId", awardReportTrackingIds);
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("Exception in deleteAwardReportTrackingById : {}", e.getMessage());
		}
	}

	@Override
	public List<AwardAttachmentType> fetchAllAwardAttachmentTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAttachmentType> query = builder.createQuery(AwardAttachmentType.class);
		Root<AwardAttachmentType> attachmentType = query.from(AwardAttachmentType.class);
		query.orderBy(builder.asc(attachmentType.get(Constants.DESCRIPTION)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardAttachment> getAwardAttachmentsByAwardId(Integer awardId, Boolean isPersonHasPermission) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAttachment> query = builder.createQuery(AwardAttachment.class);
		Root<AwardAttachment> awardAttachment = query.from(AwardAttachment.class);
		Predicate isPrivate = null;
		Predicate predicateAwardId = builder.equal(awardAttachment.get(AWARDID), awardId);
		if (Boolean.FALSE.equals(isPersonHasPermission)) {
			isPrivate = builder.equal(awardAttachment.get("attachmentType").get("isPrivate"), false);
			query.where(builder.and(predicateAwardId, isPrivate));
		} else {
			query.where(predicateAwardId);
		}
		query.orderBy(builder.asc(awardAttachment.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardAttachment fetchAwardAttachmentById(Integer awardAttachmentId) {
		return hibernateTemplate.get(AwardAttachment.class, awardAttachmentId);
	}

	@Override
	public List<AwardAttachment> fetchSortedAwardAttachments(Integer awardId, String sortBy, String reverse) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAttachment> query = builder.createQuery(AwardAttachment.class);
		Root<AwardAttachment> attachmentRoot = query.from(AwardAttachment.class);
		Predicate predicate1 = builder.equal(attachmentRoot.get(AWARDID), awardId);
		query.where(builder.and(predicate1));
		if (sortBy.equals("typeCode")) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get("attachmentType").get(DESCRIPTION)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get("attachmentType").get(DESCRIPTION)));
			}
		} else if (sortBy.equals(DESCRIPTION)) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get(DESCRIPTION)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get(DESCRIPTION)));
			}
		} else if (sortBy.equals(FILE_NAME)) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get(FILE_NAME)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get(FILE_NAME)));
			}
		} else if (sortBy.equals(UPDATE_USER)) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get(UPDATE_USER)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get(UPDATE_USER)));
			}
		} else if (sortBy.equals(UPDATETIMESTAMP)) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get(UPDATETIMESTAMP)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get(UPDATETIMESTAMP)));
			}
		} else if (sortBy.equals("narrativeStatusCode")) {
			if (reverse.equals("ASC")) {
				query.orderBy(builder.asc(attachmentRoot.get("narrativeStatus").get(DESCRIPTION)));
			} else {
				query.orderBy(builder.desc(attachmentRoot.get("narrativeStatus").get(DESCRIPTION)));
			}
		}
		List <AwardAttachment> attachments = session.createQuery(query).getResultList();
		for (AwardAttachment attachment : attachments) {
			if (attachment.getUpdateUser() != null) {
				attachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(attachment.getUpdateUser()));
			}
		}
		return attachments;
	}

	@Override
	public void deleteAwardTransaction(AwardAmountInfo awardAmountInfo) {
		try {
			hibernateTemplate.delete(awardAmountInfo);
		} catch (Exception e) {
			logger.error("Exception in deleteAwardTransaction : {}", e.getMessage());
		}
	}

	@Override
	public void deleteAwardAmountTransaction(AwardAmountTransaction awardAmountTransaction) {
		try {
			hibernateTemplate.delete(awardAmountTransaction);
		} catch (Exception e) {
			logger.error("Exception in deleteAwardAmountTransaction : {}", e.getMessage());
		}
	}

	@Override
	public AwardPerson saveOrUpdateAwardPersons(AwardPerson awardPerson) {
		try {
			hibernateTemplate.saveOrUpdate(awardPerson);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardPersons : {}", e.getMessage());
			throw new ApplicationException("Error in saveOrUpdateAwardPersons", e, Constants.JAVA_ERROR);
		}
		return awardPerson;
	}

	@Override
	public AwardPersonUnit saveOrUpdateAwardPerson(AwardPersonUnit awardPersonUnit) {
		try {
			hibernateTemplate.saveOrUpdate(awardPersonUnit);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardPerson : {}", e.getMessage());
		}
		return awardPersonUnit;
	}

	@Override
	public List<AwardHierarchyDto> fetchAllawardHierarchy(String awardNumber, String selectedAwardNumber) {
		AwardHierarchyVO awardHierarchyVO = new AwardHierarchyVO();
		return getAllAwardHierarchyDetails(awardHierarchyVO, selectedAwardNumber);
	}

	private List<AwardHierarchyDto> getAllAwardHierarchyDetails(AwardHierarchyVO awardHierarchyVO, String selectedAwardNumber) {
		List<AwardHierarchyDto> returnAwardHierarchy = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection con = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("Y")) {
				String aProcedureName = "get_award_hierarchy";
				String functionCall = "{call " + aProcedureName + "(?,?)}";
				callstm = con.prepareCall(functionCall);
				callstm.registerOutParameter(1, OracleTypes.CURSOR);
				callstm.setString(2, selectedAwardNumber);
				callstm.execute();
				rset = (ResultSet) callstm.getObject(1);
			} else if (oracledb.equalsIgnoreCase("N")) {
				callstm = con.prepareCall("{call get_award_hierarchy(?)}");
				callstm.setString(1, selectedAwardNumber);
				callstm.execute();
				rset = callstm.getResultSet();
			}
			while (rset.next()) {
				AwardHierarchyDto awardHierarchy = new AwardHierarchyDto();
				awardHierarchy.setAwardNumber((String) rset.getString(AWARD_NUMBER));
				awardHierarchy.setSelected(selectedAwardNumber.equalsIgnoreCase((String) rset.getString(AWARD_NUMBER)) ? true : false);
				awardHierarchy.setOpen(true);
				awardHierarchy.setParentAwardNumber((String) rset.getString(PARENT_AWARD_NUMBER));
				awardHierarchy.setPrincipalInvestigator((String) rset.getString(PI_NAME));
				awardHierarchy.setAccountNumber((String) rset.getString(ACCOUNT_NUMBER));
				awardHierarchy.setStatusCode(Integer.parseInt((rset.getString(STATUSCODE).toString())));
				awardHierarchy.setAwardId(rset.getString(AWARD_ID));
				awardHierarchy.setRootAwardNumber(rset.getString(ROOT_AWARD_NUMBER));
				String accountNumber;
				String piName;
				if ((String) rset.getString(ACCOUNT_NUMBER) == null) {
					accountNumber = "";
				} else {
					accountNumber = " : " + (String) rset.getString(ACCOUNT_NUMBER);
				}
				if ((String) rset.getString(PI_NAME) == null) {
					piName = "";
				} else {
					piName = " : " + (String) rset.getString("pi_name");
				}
				awardHierarchy.setName((String) rset.getString(AWARD_NUMBER) + accountNumber + piName);
				returnAwardHierarchy.add(awardHierarchy);
			}
		} catch (SQLException e) {
			logger.error("Exception in getAllAwardHierarchyDetails : {}", e.getMessage());
		}
		return returnAwardHierarchy;
	}

	@Override
	public Award fetchAwardByAwardId(String awardId) {
		return hibernateTemplate.get(Award.class, Integer.parseInt(awardId));
	}

	@Override
	public String saveOrUpdateAwardHierarchy(AwardHierarchy awardHierarchy) {
		try {
			hibernateTemplate.saveOrUpdate(awardHierarchy);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardHierarchy : {}", e.getMessage());
			throw new ApplicationException("Error occurred in saveOrUpdateAwardHierarchy", e, Constants.JAVA_ERROR);
		}
		return "success";
	}

	@Override
	public List<AwardPerson> fetchAllAwardPesonsByAwardId(String awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardPerson> query = builder.createQuery(AwardPerson.class);
		Root<AwardPerson> awardPersons = query.from(AwardPerson.class);
		query.where(builder.equal(awardPersons.get(AWARDID), awardId));
		query.orderBy(builder.asc(awardPersons.get("fullName")));
		return session.createQuery(query).setMaxResults(20).getResultList();
	}

	@Override
	public String getNextChildAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		String result = null;
		try {
			String functionName = "FN_GET_AWARD_HIERARCHY_NEXTSEQ";
			String functionCall = "{ ? = call " + functionName + "(?) }";
			statement = connection.prepareCall(functionCall);
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setString(2, awardNumber);
			statement.execute();
			result = statement.getString(1);
		} catch (SQLException e) {
			logger.error("Exception in getNextChildAwardNumber : {}", e.getMessage());
			throw new ApplicationException("Error in getNextChildAwardNumber", e, Constants.DB_FN_ERROR);
		}
		return result;
	}

	@Override
	public List<AwardHierarchy> getChildAwards(String parntAwardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardHierarchy> query = builder.createQuery(AwardHierarchy.class);
		Root<AwardHierarchy> childAwards = query.from(AwardHierarchy.class);
		query.where(builder.equal(childAwards.get("parentAwardNumber"), parntAwardNumber));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String getAwardName(Integer awardId) {
		String awardTitle = "";
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT title FROM Award WHERE awardId=:awardId";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter(AWARDID, awardId);
		awardTitle = query.uniqueResult();
		return awardTitle;
	}

	@Override
	public void deleteChildAwards(AwardHierarchy awardHierarchy) {
		try {
			hibernateTemplate.delete(awardHierarchy);
		} catch (Exception e) {
			logger.error("Exception in deleteChildAwards : {}", e.getMessage());
		}
	}

	@Override
	public void deleteAwardByAwardNumber(String awardNumber) {
		try {
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery("delete from Award d where d.awardNumber = :awardNumber");
			query.setParameter(AWARDNUMBER, awardNumber);
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("Exception in deleteAwardByAwardNumber : {}", e.getMessage());
		}
	}

	@Override
	public List<AwardHierarchy> getAwardHierarchyByAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardHierarchy> query = builder.createQuery(AwardHierarchy.class);
		Root<AwardHierarchy> childAwards = query.from(AwardHierarchy.class);
		query.where(builder.equal(childAwards.get(AWARDNUMBER), awardNumber));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Award saveAward(Award award) {
		try {
			hibernateTemplate.save(award);
		} catch (Exception e) {
			logger.error("Exception in saveAward : {}", e.getMessage());
		}
		return award;
	}

	@Override
	public AwardStatus fetchAwardStatusByCode(String statusCode) {
		return hibernateTemplate.get(AwardStatus.class, statusCode);
	}

	@Override
    public AwardView fetchAwardViewByAwardId(Integer awardId) {
            return hibernateTemplate.get(AwardView.class, awardId);
    }

	@Override
	public void deleteAwardProjectTeam(Integer awardPersonId) {
		hibernateTemplate.delete(hibernateTemplate.get(AwardProjectTeam.class, awardPersonId));
	}

	@Override
	public List<AwardAttachment> fetchAwardAttachmentBasedOnAwardIdAndDocumentId(Integer awardId, Integer documentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAttachment> query = builder.createQuery(AwardAttachment.class);
		Root<AwardAttachment> rootAwardAttachment = query.from(AwardAttachment.class);
		Predicate predicateOne = builder.equal(rootAwardAttachment.get(AWARDID),awardId);
		Predicate predicateTwo = builder.equal(rootAwardAttachment.get("documentId"),documentId);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardContact saveOrUpdateAwardContact(AwardContact awardContact) {
		try {
			hibernateTemplate.saveOrUpdate(awardContact);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardContact : {}", e.getMessage());
			throw new ApplicationException("Error occurred in saveOrUpdateAwardContact", e, Constants.JAVA_ERROR);
		}
		return awardContact;
	}

	@Override
	public void deleteAwardContact(Integer awardPersonId) {
		hibernateTemplate.delete(hibernateTemplate.get(AwardContact.class, awardPersonId));		
	}

	@Override
	public AwardPerson getAwardPersonById(Integer awardPersonalId) {
		return hibernateTemplate.get(AwardPerson.class, awardPersonalId);
	}

	@Override
	public AwardPersonAttachment fetchAwardPersonAttachmentById(Integer attachmentid) {
		return hibernateTemplate.get(AwardPersonAttachment.class, attachmentid);
	}

	@Override
	public AwardType fetchAwardTypeByAwardTypeCode(String awardTypeCode) {
		return hibernateTemplate.get(AwardType.class, awardTypeCode);
	}

	@Override
	public AwardDocumentType fetchAwardDocumentTypeById(String awardDocumentTypeCode) {
		return hibernateTemplate.get(AwardDocumentType.class, awardDocumentTypeCode);
	}

	@Override
	public List<AwardReportTerms> getAwardReportTermsByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardReportTerms> query = builder.createQuery(AwardReportTerms.class);
		Root<AwardReportTerms> rootAwardReportTerms = query.from(AwardReportTerms.class);
		query.where(builder.equal(rootAwardReportTerms.get(AWARDID), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Award fetchAwardByAwardNumberAndSequenceNumber(String awardNumber, int sequenceNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		Predicate predicateTwo = builder.equal(rootAward.get(SEQUENCENUMBER), sequenceNumber);
		Predicate predicateThree = builder.equal(rootAward.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ACTIVE);
		Predicate predicatefour = builder.equal(rootAward.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ARCHIVE);
		Predicate predicateFive = builder.or(predicateThree, predicatefour);
		query.where(builder.and(predicateOne, predicateTwo, predicateFive));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public List<AwardSponsorTerm> getAwardSponsorTermByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardSponsorTerm> query = builder.createQuery(AwardSponsorTerm.class);
		Root<AwardSponsorTerm> rootAwardSponsorTerm = query.from(AwardSponsorTerm.class);
		query.where(builder.equal(rootAwardSponsorTerm.get(AWARDID), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Award> fetchAwardByAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		query.where(builder.and(predicateOne));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Award fetchAwardByAwardNumberAndAwardSequenceStatus(String awardNumber, List<String> awardSequenceStatuses) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateAwardNumber = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		Predicate predicateAwardSequenceStatus = rootAward.get(AWARDSEQUENCESTATUS).in(awardSequenceStatuses);
		query.where(builder.and(predicateAwardNumber, predicateAwardSequenceStatus));
		try {
			return session.createQuery(query).uniqueResult();
		} catch (Exception e) {
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardSummaryDetailsVO> fetchAwardByAwardNumbersAndAwardSequenceStatus(String awardNumber, List<String> awardSequenceStatuses) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		List<AwardSummaryDetailsVO> awardSummaryDetails = new ArrayList<>();
		Query query = session.createQuery("SELECT T1.awardId, T1.awardSequenceStatus, T1.awardNumber, T1.sequenceNumber, T1.awardVariationTypeCode,"
				+ "T1.createUser, T1.title, T2.description as awardVariationName, T1.createTimestamp FROM Award T1\r\n" + 
				"LEFT JOIN ServiceRequestType T2 ON T1.awardVariationTypeCode = T2.typeCode\r\n" + 
				"WHERE T1.awardNumber = :awardNumber AND T1.awardSequenceStatus IN(:awardSequenceStatuses)");
		query.setParameter(AWARDNUMBER, awardNumber);
		query.setParameter("awardSequenceStatuses", awardSequenceStatuses);
		List<Object[]> awards = query.getResultList();
		awards.stream().forEach(award -> {
			AwardSummaryDetailsVO awardSummaryDetail = new AwardSummaryDetailsVO();
			if (award[0] != null) {
				awardSummaryDetail.setAwardId(Integer.parseInt(award[0].toString()));
			}
			if (award[1] != null) {
				awardSummaryDetail.setAwardSequenceStatus(award[1].toString());
			}
			if (award[2] != null) {
				awardSummaryDetail.setAwardNumber(award[2].toString());
			}
			if (award[3] != null) {
				awardSummaryDetail.setSequenceNumber(Integer.parseInt(award[3].toString()));
			}
			if (award[4] != null) {
				awardSummaryDetail.setAwardVariationTypeCode(award[4].toString());
			}
			if (award[5] != null) {
				awardSummaryDetail.setCreateUser(award[5].toString());
			}
			if (award[6] != null) {
				awardSummaryDetail.setTitle(award[6].toString());
			}
			if (award[7] != null) {
				awardSummaryDetail.setAwardVariationName(award[7].toString());
			}
			if (award[8] != null) {
				awardSummaryDetail.setCreateTimestamp((Timestamp) award[8]);
			}
			awardSummaryDetails.add(awardSummaryDetail);
		});
		return awardSummaryDetails;
	}

	@Override
	public List<AwardHierarchy> checkAwardHierarchyExisted(Award award) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardHierarchy> query = builder.createQuery(AwardHierarchy.class);
		Root<AwardHierarchy> attachmentRoot = query.from(AwardHierarchy.class);
		Predicate predicate1 = builder.equal(attachmentRoot.get(AWARDNUMBER), award.getAwardNumber());
		query.where(builder.and(predicate1));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ServiceRequest getServiceRequestBasedOnAwardId(String awardId, String originatedAwardId) {
		ServiceRequest serviceRequest = null;
		try {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequest> query = builder.createQuery(ServiceRequest.class);
		Root<ServiceRequest> rootServiceRequest = query.from(ServiceRequest.class);
		Predicate predicateOne = rootServiceRequest.get("moduleItemKey").in(awardId);
		Predicate predicateTwo = builder.equal(rootServiceRequest.get("isSystemGenerated"), Boolean.TRUE);
		query.where(builder.and(predicateOne, predicateTwo));
		serviceRequest = session.createQuery(query).uniqueResult();
		if (serviceRequest == null) {
			serviceRequest = getServiceRequestBasedOnOriginatedAwardId(originatedAwardId);
		}
		return serviceRequest;
		} catch (NonUniqueResultException e) {
			if (originatedAwardId != null) {
				serviceRequest = getServiceRequestBasedOnOriginatedAwardId(originatedAwardId);
			}
			if (serviceRequest == null) {
				serviceRequest = getServiceRequestBasedonActiveAward(awardId);
			}
			return serviceRequest;
		}
	}
	
	private ServiceRequest getServiceRequestBasedonActiveAward(String awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequest> query = builder.createQuery(ServiceRequest.class);
		Root<ServiceRequest> rootServiceRequest = query.from(ServiceRequest.class);
		Predicate predicateOne = rootServiceRequest.get("moduleItemKey").in(awardId);
		Predicate predicateTwo = builder.equal(rootServiceRequest.get("isSystemGenerated"), Boolean.TRUE);
		Predicate predicateThree = rootServiceRequest.get("originatingModuleItemKey").isNull();
		query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public List<SponsorReport> fetchSponsorReportBasedOnFundingSourceType(Integer fundingSchemeId, String sponsorCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorReport> query = builder.createQuery(SponsorReport.class);
		Root<SponsorReport> rootSponsorReport = query.from(SponsorReport.class);
		Predicate predicate1 = builder.equal(rootSponsorReport.get(FUNDINGSCHEMEID), fundingSchemeId);
		Predicate predicate2 = builder.equal(rootSponsorReport.get(SPONSORCODE), sponsorCode);
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SponsorTermReport> fetchSponsorTermReportBasedOnFundingSourceType(Integer fundingSchemeId, String sponsorCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorTermReport> query = builder.createQuery(SponsorTermReport.class);
		Root<SponsorTermReport> rootSponsorReport = query.from(SponsorTermReport.class);
		Predicate predicate1 = builder.equal(rootSponsorReport.get(FUNDINGSCHEMEID), fundingSchemeId);
		Predicate predicate2 = builder.equal(rootSponsorReport.get(SPONSORCODE), sponsorCode);
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SponsorReport> fetchSponsorReportBasedOnSponsor(String sponsorCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorReport> query = builder.createQuery(SponsorReport.class);
		Root<SponsorReport> rootSponsorReport = query.from(SponsorReport.class);
		Predicate predicate1 = builder.equal(rootSponsorReport.get(SPONSORCODE), sponsorCode);
		Predicate predicate2 = builder.isNull((rootSponsorReport.get(FUNDINGSCHEMEID)));
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<SponsorTermReport> fetchSponsorTermReportBasedOnSponsor(String sponsorCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorTermReport> query = builder.createQuery(SponsorTermReport.class);
		Root<SponsorTermReport> rootSponsorReport = query.from(SponsorTermReport.class);
		Predicate predicate1 = builder.equal(rootSponsorReport.get(SPONSORCODE), sponsorCode);
		Predicate predicate2 = builder.isNull((rootSponsorReport.get(FUNDINGSCHEMEID)));
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer getProposalId(String ipNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT proposalId FROM InstituteProposal WHERE proposalNumber=:ipNumber AND sequenceNumber IN (SELECT MAX(sequenceNumber)\n" + 
				"FROM InstituteProposal\n" + 
				"WHERE  proposalNumber=:ipNumber)";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter("ipNumber", ipNumber);
		return query.uniqueResult();
	}

	@Override
	public void deleteAwardPersonAttachment(Integer attachmentId) {
		hibernateTemplate.delete(fetchPersonAttachmentById(attachmentId));
	}

	@Override
	public AwardPersonAttachment fetchPersonAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(AwardPersonAttachment.class, attachmentId);
	}

	@Override
	public void deleteAwardPersonUnit(AwardPersonUnit awardPersonUnit) {
		try {
			hibernateTemplate.delete(awardPersonUnit);
		} catch (Exception e) {
			logger.error("Exception in deleteAwardPersonUnit : {}", e.getMessage());
		}
	}

	@Override
	public List<Award> showAwardHistory(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicate1 = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		query.where(builder.and(predicate1));
		query.orderBy(builder.desc(rootAward.get(SEQUENCENUMBER)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardApprovedEquipment> getAwardApprovedEquipmentByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardApprovedEquipment> query = builder.createQuery(AwardApprovedEquipment.class);
		Root<AwardApprovedEquipment> rootAwardApprovedEquipment = query.from(AwardApprovedEquipment.class);
		query.where(builder.equal(rootAwardApprovedEquipment.get(AWARDID), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardAprovedForeignTravel> getAwardAprovedForeignTravelByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAprovedForeignTravel> query = builder.createQuery(AwardAprovedForeignTravel.class);
		Root<AwardAprovedForeignTravel> rootAwardAprovedForeignTravel = query.from(AwardAprovedForeignTravel.class);
		query.where(builder.equal(rootAwardAprovedForeignTravel.get(AWARDID), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardPersonRoles> fetchAwardPersonRoles(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardPersonRoles> query = builder.createQuery(AwardPersonRoles.class);
		Root<AwardPersonRoles> rootAwardPersonRoles = query.from(AwardPersonRoles.class);
		query.where(builder.equal(rootAwardPersonRoles.get(AWARDID), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Role> fetchAwardRoles() {
		List<Integer> roles = new ArrayList<>();
		roles.add(Constants.CREATE_VARIATION_REQUEST);
//		roles.add(Constants.INITIATE_PROJECT_CLOSSURE);
//		roles.add(Constants.MODIFY_AWARD_OUTCOME);
//		roles.add(Constants.WITHDRAW_AWARD);
		roles.add(Constants.VIEW_AWARD_ROLE_ID);
		roles.add(Constants.MODIFY_AWARD);
		return rolesManagementDao.getRoleBasedOnRoleId(roles);
	}

	@Override
	public AwardPersonRoles saveOrUpdateAwardPersonRoles(AwardPersonRoles awardPersonRole) {
		hibernateTemplate.saveOrUpdate(awardPersonRole);
		return awardPersonRole;
	}

	@Override
	public AwardPersonRoles deleteAwardPersonRoles(AwardPersonRoles awardPersonRole) {
		try {
			hibernateTemplate.delete(awardPersonRole);
		} catch (Exception e) {
			logger.error("Exception in deleteAwardPersonRoles : {}", e.getMessage());
		}
		return awardPersonRole;
	}

	@Override
	public List<AwardPersonRoles> fetchAwardPersonRolesByParams(String personId, Integer awardId, List<Integer> roleIds) {
		List<AwardPersonRoles> awardPersonRoles = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardPersonRoles> query = builder.createQuery(AwardPersonRoles.class);
		Root<AwardPersonRoles> rootAwardPersonRoles = query.from(AwardPersonRoles.class);
		Predicate predicateOne = builder.equal(rootAwardPersonRoles.get("personId"), personId);
		Predicate predicateTwo = builder.equal(rootAwardPersonRoles.get(AWARDID), awardId);
		if (roleIds != null && !roleIds.isEmpty()) {
			Predicate predicateThree = rootAwardPersonRoles.get("roleId").in(roleIds);
			query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		} else {
			query.where(builder.and(predicateOne, predicateTwo));
		}
		awardPersonRoles = session.createQuery(query).getResultList();
		if (awardPersonRoles != null && !awardPersonRoles.isEmpty()) {
			return awardPersonRoles;
		}
		return awardPersonRoles;
	}

	@Override
	public AwardPersonRoles fetchAwardPersonRoleBasedonAwardPersonRoleId(Integer awardPersonalId) {
		return hibernateTemplate.get(AwardPersonRoles.class, awardPersonalId);
	}

	@Override
	public AwardReportTrackingFile saveOrUpdateAwardReportTrackingFile(AwardReportTrackingFile awardReportTrackingFile) {
		hibernateTemplate.saveOrUpdate(awardReportTrackingFile);
		return awardReportTrackingFile;
	}

	@Override
	public List<AwardReportTrackingFile> fetchAwardReportTrackingFileBasedOnAwardReportTrackingId(Integer awardReportTrackingId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardReportTrackingFile> query = builder.createQuery(AwardReportTrackingFile.class);
		Root<AwardReportTrackingFile> rootAwardReportTrackingFile = query.from(AwardReportTrackingFile.class);
		query.where(builder.equal(rootAwardReportTrackingFile.get("awardReportTrackingId"), awardReportTrackingId));
		query.orderBy(builder.desc(rootAwardReportTrackingFile.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardReportTrackingFile getAwardReportTrackingFileByFileId(Integer awardReportTrackingFileId) {
		return hibernateTemplate.get(AwardReportTrackingFile.class, awardReportTrackingFileId);
	}

	@Override
	public Award getAwardFromSequenceIdAndAwardNumber(Integer sequenceNumber, String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicate1 = builder.equal(rootAward.get(SEQUENCENUMBER), sequenceNumber);
		Predicate predicate2 = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public AwardReportTrackingFile deleteAwardReportTrackingFile(AwardReportTrackingFile awardReportTrackingFile) {
		try {
			hibernateTemplate.delete(awardReportTrackingFile);
		} catch (Exception e) {
			logger.error("Exception in deleteAwardReportTrackingFile : {}", e.getMessage());
		}
		return awardReportTrackingFile;
	}

	@Override
	public Integer getMaxSequenceNumberBasedOnAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER),awardNumber);
		query.select(builder.max(rootAward.get(SEQUENCENUMBER)));
		query.where(builder.and(predicateOne));
		if(session.createQuery(query).getSingleResult() != null) {
			return  session.createQuery(query).getSingleResult()+1;
		} else {
			return 1;
		}
	}

	@Override
	public Award fetchActiveAwardByAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		Predicate predicateThree = builder.equal(rootAward.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ACTIVE);
		query.where(builder.and(predicateOne, predicateThree));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public String getAwardNumberBasedOnAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT awardNumber FROM Award WHERE awardId=:awardId";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter(AWARDID, awardId);
		return query.uniqueResult();
	}

	@Override
	public List<ValidReportClass> fetchValidReportClassByReportClassCode(String reportClassCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "FROM ValidReportClass WHERE reportClassCode=:reportClassCode";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<ValidReportClass> query = session.createQuery(hqlQuery);
		query.setParameter(REPORTCLASSCODE, reportClassCode);
		return query.getResultList();
	}

	@Override
	public String getReportNameByReportCode(String reportCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT description FROM Report WHERE reportCode=:reportCode";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter(REPORTCODE, reportCode);
		return query.uniqueResult();
	}

	@Override
	public AwardTransactionType getAwardTransactionTypeById(Integer awardTransactionTypeCode) {
		return hibernateTemplate.get(AwardTransactionType.class, awardTransactionTypeCode);
	}

	@Override
	public Integer getProposalIdByIpNumber(String ipNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT proposalId FROM Proposal WHERE ipNumber=:ipNumber";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter("ipNumber", ipNumber);
		return query.uniqueResult();
	}

	@Override
	public Award fetchPendingAwardByAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		Predicate predicateThree = builder.equal(rootAward.get(AWARDSEQUENCESTATUS),
				Constants.AWARD_FINAL_STATUS_PENDING);
		query.where(builder.and(predicateOne, predicateThree));
		try {
			return session.createQuery(query).uniqueResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public Award fetchAwardByParams(String awardNumber, int sequenceNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		Predicate predicateTwo = builder.equal(rootAward.get(SEQUENCENUMBER), sequenceNumber);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public List<Award> fetchAllHoldAwards() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Timestamp currentDate = commonDao.getCurrentTimestamp();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> parameter = query.from(Award.class);
		Predicate predicate1 = builder.lessThanOrEqualTo(parameter.get("beginDate"), currentDate);
		Predicate predicate2 = builder.equal(parameter.get("statusCode"), Constants.AWARD_STATUS_CODE_HOLD);
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardReportReminder> fetchAllActiveAwardReportReminder() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardReportReminder> query = builder.createQuery(AwardReportReminder.class);
		Root<AwardReportReminder> parameter = query.from(AwardReportReminder.class);
		Predicate predicate1 = builder.equal(parameter.get("isActive"), true);
		query.where(builder.and(predicate1));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardReportTerms> fetchAllAwardReportTermsBasedOnParams(String reportClassCode, String reportCode, String frequencyCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardReportTerms> query = builder.createQuery(AwardReportTerms.class);
		Root<AwardReportTerms> parameter = query.from(AwardReportTerms.class);
		Predicate predicate1 = builder.equal(parameter.get(REPORTCLASSCODE), reportClassCode);
		Predicate predicate2 = builder.equal(parameter.get(REPORTCODE), reportCode);
		Predicate predicate3 = builder.equal(parameter.get("frequencyCode"), frequencyCode);
		Predicate predicate4 = builder.equal(parameter.get("award").get("awardSequenceStatus"), Constants.AWARD_FINAL_STATUS_ACTIVE);
		Predicate predicate5 = builder.equal(parameter.get("award").get("awardDocumentTypeCode"), Constants.AWARD_SETUP);
		Predicate predicate6 = builder.equal(parameter.get("award").get("awardSequenceStatus"), Constants.AWARD_FINAL_STATUS_PENDING);
		Predicate predicate7 = builder.and(predicate5, predicate6);
		Predicate predicate8 = builder.or(predicate4, predicate7);
		query.where(builder.and(predicate1, predicate2, predicate3, predicate8));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardMethodOfPayment> getAwardMethodOfPaymentList() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardMethodOfPayment> query = builder.createQuery(AwardMethodOfPayment.class);
		Root<AwardMethodOfPayment> rootAgreementSponsorType = query.from(AwardMethodOfPayment.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardBasisOfPayment> getAwardBasisOfPaymentList() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardBasisOfPayment> query = builder.createQuery(AwardBasisOfPayment.class);
		Root<AwardBasisOfPayment> rootAgreementSponsorType = query.from(AwardBasisOfPayment.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardMethodOfPayment getAwardMethodOfPaymentById(String methodOfPaymentCode) {
		return hibernateTemplate.get(AwardMethodOfPayment.class, methodOfPaymentCode);
	}

	@Override
	public AwardBasisOfPayment getAwardBasisOfPaymentById(String basisOfPaymentCode) {
		return hibernateTemplate.get(AwardBasisOfPayment.class, basisOfPaymentCode);
	}

	@Override
	public List<AwardMileStone> fetchAwardMileStonesBasedOnAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardMileStone> query = builder.createQuery(AwardMileStone.class);
		Root<AwardMileStone> rootAwardMileStone = query.from(AwardMileStone.class);
		query.where(builder.equal(rootAwardMileStone.get(AWARDID), awardId));
		query.orderBy(builder.desc(rootAwardMileStone.get(UPDATETIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardMileStone saveOrUpdateAwardMileStone(AwardMileStone awardMileStone) {
		try {
			hibernateTemplate.saveOrUpdate(awardMileStone);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardMileStone : {}", e.getMessage());
			throw new ApplicationException("Error in saveOrUpdateAwardMileStone", e, Constants.JAVA_ERROR);
		}
		return awardMileStone;
	}

	@Override
	public String deleteAwardMilestone(Integer awardMilestoneId) {
		hibernateTemplate.delete(hibernateTemplate.get(AwardMileStone.class, awardMilestoneId));
		return "Award Milestone deleted successfully";
	}

	@Override
	public AwardKPI saveOrUpdateAwardKPI(AwardKPI awardKPI) {
		try {
			hibernateTemplate.saveOrUpdate(awardKPI);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateAwardKPI: {} ", e.getMessage());
			throw new ApplicationException("Error in saveOrUpdateAwardKPI", e, Constants.JAVA_ERROR);
		}
		return awardKPI;
	}

	@Override
	public List<AwardKPI> fetchAllAwardKPI(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardKPI> query = builder.createQuery(AwardKPI.class);
		Root<AwardKPI> rootProposalKpi = query.from(AwardKPI.class);
		Predicate predicateOne = builder.equal(rootProposalKpi.get(AWARDID), awardId);
		query.where(builder.and(predicateOne));
		query.orderBy(builder.asc(rootProposalKpi.get("kpiTypeCode")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String deleteAwardKPI(Integer awardKPIId, Integer awardId, Integer awardKPICriteriaId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardKPI> query = builder.createQuery(AwardKPI.class);
		Root<AwardKPI> rootAwardKPI = query.from(AwardKPI.class);
		Predicate predicate1 = builder.equal(rootAwardKPI.get(AWARDID), awardId);
		query.where(builder.and(predicate1));
		if (awardKPICriteriaId == null && awardKPIId == null && awardId != null) {
			List<AwardKPI> awardKPI = session.createQuery(query).getResultList();
			hibernateTemplate.deleteAll(awardKPI);
		} else if (awardKPIId != null && awardKPICriteriaId == null && awardId != null) {
			hibernateTemplate.delete(hibernateTemplate.get(AwardKPI.class, awardKPIId));
		} else if (awardKPICriteriaId != null) {
			hibernateTemplate.delete(hibernateTemplate.get(AwardKPICriteria.class, awardKPICriteriaId));
		}
		return "Award KPI deleted successfully";
	}

	@SuppressWarnings("unchecked")
	@Override
	public Set<Integer> fetchAwardIdsByAwardNumberAndNotInSequenceStatus(String awardNumber, String sequenceStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Set<Integer> awardIds = new HashSet<Integer>();
 		String hqlQuery = "SELECT A.awardId FROM Award A WHERE A.awardNumber = :awardNumber and A.awardSequenceStatus <> :awardSequenceStatus";
		Query query = session.createQuery(hqlQuery);
		query.setParameter(AWARDNUMBER, awardNumber);
		query.setParameter(AWARDSEQUENCESTATUS, sequenceStatus);
		if (query.getResultList() != null && !query.getResultList().isEmpty()) {
			awardIds.addAll(query.getResultList());
		}	
        return awardIds;
	}

	@Override
	public List<AwardAttachment> fetchAwardAttachmentBasedOnAttachmentIds(List<Integer> attachmentIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builderAwardAttachment = session.getCriteriaBuilder();
		CriteriaQuery<AwardAttachment> queryAwardAttachment = builderAwardAttachment.createQuery(AwardAttachment.class);
		Root<AwardAttachment> rootAwardAttachment = queryAwardAttachment.from(AwardAttachment.class);
		queryAwardAttachment.where(rootAwardAttachment.get("awardAttachmentId").in(attachmentIds));
		return session.createQuery(queryAwardAttachment).getResultList();
	}

	@Override
	public AwardResearchArea saveOrUpdateAwardResearchArea(AwardResearchArea awardResearchArea) {
			try {
				hibernateTemplate.saveOrUpdate(awardResearchArea);
			} catch (Exception e) {
				logger.error("Exception in saveOrUpdateAwardResearchArea: {} ", e.getMessage());
				throw new ApplicationException("Error in saveOrUpdateAwardResearchArea", e, Constants.JAVA_ERROR);
			}
		return awardResearchArea;
	}

	@Override
	public List<AwardResearchArea> fetchAwardResearchAreaBasedOnAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardResearchArea> query = builder.createQuery(AwardResearchArea.class);
		Root<AwardResearchArea> rootAwardResearchArea = query.from(AwardResearchArea.class);
		query.where(builder.equal(rootAwardResearchArea.get(AWARDID), awardId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardResearchArea fetchAwardResearchArea(Integer researchAreaId) {
		return hibernateTemplate.get(AwardResearchArea.class, researchAreaId);
	}

	@Override
	public AwardResearchArea deleteAwardResearchArea(AwardResearchArea awardResearchArea) {
		try {
			hibernateTemplate.delete(awardResearchArea);
		} catch (Exception e) {
			logger.error("Exception in deleteProposalResearchArea: {} ", e.getMessage());
		}
		return awardResearchArea;
	}

	@Override
	public List<Award> fetchAwardByStatusCode(String awardWorkflowStatusCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> award = builder.createQuery(Award.class);
		Root<Award> rootAward = award.from(Award.class);
		award.where(builder.equal(rootAward.get("workflowAwardStatusCode"), awardWorkflowStatusCode));
		return session.createQuery(award).getResultList();
	}

	@Override
	public AwardWorkflowStatus fetchAwardWorkflowStatusByStatusCode(String awardWorkflowStatusCode) {
		return hibernateTemplate.get(AwardWorkflowStatus.class, awardWorkflowStatusCode);
	}

	@Override
	public Award fetchGrantCallByAwardId(Integer awardId) {
		return hibernateTemplate.get(Award.class, awardId);
	}

	@Override
	public List<AwardPersonRoles> fetchAwardPersonRoles(Integer awardId, Integer roleId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardPersonRoles> query = builder.createQuery(AwardPersonRoles.class);
		Root<AwardPersonRoles> rootAwardPersonRoles = query.from(AwardPersonRoles.class);
		Predicate predicateAwardId = builder.equal(rootAwardPersonRoles.get(AWARDID), awardId);
		Predicate predicateRoleId = builder.equal(rootAwardPersonRoles.get("roleId"), roleId);
		if (roleId != null) {
			query.where(builder.and(predicateAwardId, predicateRoleId));
		} else {
			query.where(builder.and(predicateAwardId));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public String fetchAwardLeadUnitNumberByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT leadUnitNumber FROM Award WHERE awardId=:awardId";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter(AWARDID, awardId);
		return query.getSingleResult();
	}

	@Override
	public AwardComment saveOrUpdateAwardComment(AwardComment awardComment) {
			try {
				hibernateTemplate.saveOrUpdate(awardComment);
			} catch (Exception e) {
				logger.error("Exception in saveOrUpdateAwardComment: {} ", e.getMessage());
			}
		return awardComment;
	}

	@Override
	public Integer getAwardSequenceNumberBasedOnAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT sequenceNumber FROM Award WHERE awardId=:awardId";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter(AWARDID, awardId);
		return query.uniqueResult();
	}

	@Override
	public List<Integer> getPreviousAwardIdsBasedOnParams(String awardNumber, Integer sequenceNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT awardId FROM Award WHERE awardNumber=:awardNumber and statusCode not in (11,13) and sequenceNumber <:sequenceNumber";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<Integer> query = session.createQuery(hqlQuery);
		query.setParameter(AWARDNUMBER, awardNumber);
		query.setParameter(SEQUENCENUMBER, sequenceNumber);
		return query.getResultList();
	}

	@Override
	public List<AwardAttachment> getAwardAttachmentsByAwardIds(List<Integer> awardIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAttachment> query = builder.createQuery(AwardAttachment.class);
		Root<AwardAttachment> parameter = query.from(AwardAttachment.class);
		Predicate predicate1 = (builder.in(parameter.get(AWARDID)).value(awardIds));
		query.where(builder.and(predicate1));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardReportTracking> fetchAwardReportTrackingBasedOnReportTermIds(Set<Integer> reportTermIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardReportTracking> query = builder.createQuery(AwardReportTracking.class);
		Root<AwardReportTracking> rootAwardReportTracking = query.from(AwardReportTracking.class);
		Predicate predicate = rootAwardReportTracking.get("awardReportTerms").get("awardReportTermsId").in(reportTermIds);
		query.where(builder.and(predicate));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AwardPersonAttachment> fetchAwardPersonAttachmentBasedOnAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builderAwardPersonAttachment = session.getCriteriaBuilder();
		CriteriaQuery<AwardPersonAttachment> queryAwardAttachment = builderAwardPersonAttachment.createQuery(AwardPersonAttachment.class);
		Root<AwardPersonAttachment> rootAwardAttachment = queryAwardAttachment.from(AwardPersonAttachment.class);
		Predicate predicateOne = builderAwardPersonAttachment.equal(rootAwardAttachment.get("awardPerson").get("awardId"), awardId);
		queryAwardAttachment.where(builderAwardPersonAttachment.and(predicateOne));
		return session.createQuery(queryAwardAttachment).getResultList();
	}

	@Override
	public AwardAttachmentType getAwardAttachmentTypeById(String awardAttachmentTypeId) {
		return hibernateTemplate.get(AwardAttachmentType.class, awardAttachmentTypeId);
	}

	@Override
	public ServiceRequest getServiceRequestBasedOnOriginatedAwardId(String awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ServiceRequest> query = builder.createQuery(ServiceRequest.class);
		Root<ServiceRequest> rootServiceRequest = query.from(ServiceRequest.class);
		Predicate predicateOne = rootServiceRequest.get("originatingModuleItemKey").in(awardId);
		Predicate predicateTwo = builder.equal(rootServiceRequest.get("isSystemGenerated"), Boolean.TRUE);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public List<AwardPerson> getAwardPersons(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardPerson> query = builder.createQuery(AwardPerson.class);
		Root<AwardPerson> rootAwardPerson = query.from(AwardPerson.class);
		query.where(builder.equal(rootAwardPerson.get(AWARDID), awardId));
		query.orderBy(builder.asc(rootAwardPerson.get("proposalPersonRole").get("sortId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Award fetchAwardDetailsbyAccountNumber(String accountNumber, String awardSequenceStatus) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateAccountNumber = builder.equal(rootAward.get("accountNumber"), accountNumber);
		Predicate predicateAwardSequenceStatus = builder.equal(rootAward.get(AWARDSEQUENCESTATUS), awardSequenceStatus);
		query.where(builder.and(predicateAccountNumber, predicateAwardSequenceStatus));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public String getAccountNumberByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT accountNumber FROM Award WHERE awardId=:awardId";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		return (String) query.getSingleResult();
	}

	@Override
	public Integer getMaxDocumentIdBasedOnAwardNumber(String awardNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
			Root<AwardAttachment> awardAttachment = query.from(AwardAttachment.class);
			query.select(builder.max(awardAttachment.get("documentId")));
			query.where(builder.equal(awardAttachment.get(AWARDNUMBER), awardNumber));
			Integer documentId = session.createQuery(query).getSingleResult();
			return  documentId != null ? documentId : 0;
		} catch (Exception e) {
			return 0;
		}
	}

	@Override
	public Boolean checkDocumentIdExistInAward(String awardNumber, Integer documentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAttachment> query = builder.createQuery(AwardAttachment.class);
		Root<AwardAttachment> rootAwardAttachment = query.from(AwardAttachment.class);
		Predicate predicateOne = builder.equal(rootAwardAttachment.get(AWARDNUMBER),awardNumber);
		Predicate predicateTwo = builder.equal(rootAwardAttachment.get("documentId"),documentId);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).getResultList().isEmpty() ? Boolean.FALSE : Boolean.TRUE;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardAttachment> getAwardAttachmentsBasedOnParams(Integer awardId, String awardNumber, Integer sequenceNumber, Boolean isPersonHasPermission) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String filterCondition = "";
		if (Boolean.FALSE.equals(isPersonHasPermission)) {
			filterCondition = " and t3.attachmentType.isPrivate = false ";
		}
		if (sequenceNumber > 0) {
			String attachmentQuery = new StringBuilder("from AwardAttachment where awardAttachmentId in ( ") 
					.append("select t3.awardAttachmentId from AwardAttachment t3 left outer join Award t4 on t4.awardId = t3.awardId where t3.awardAttachmentId in (")
					.append("select distinct t1.awardAttachmentId from AwardAttachment t1 inner join Award t2 on t1.awardNumber = t2.awardNumber ")
					.append("where t1.documentId not in (SELECT documentId FROM AwardAttachment where awardNumber =: awardNumber ")
					.append("and documentStatusCode = 3 and sequenceNumber <=: sequenceNumber) and ")
					.append("t1.awardNumber =: awardNumber and t1.sequenceNumber <=: sequenceNumber) ")
					.append("and (t4.awardSequenceStatus IN('ARCHIVE','ACTIVE') or t3.awardId =: awardId)")
					.append(filterCondition)
							+ ")"
					.toString();
			org.hibernate.query.Query<AwardAttachment>  query = session.createQuery(attachmentQuery);
			query.setParameter(AWARDNUMBER, awardNumber);
			query.setParameter(SEQUENCENUMBER, sequenceNumber);
			query.setParameter("awardId", awardId);
			return query.getResultList();
		} else {
			String attachmentQuery = new StringBuilder("from AwardAttachment where awardAttachmentId in ( ") 
					.append("select t3.awardAttachmentId from AwardAttachment t3 left outer join Award t4 on t4.awardId = t3.awardId where t3.awardAttachmentId in (")
					.append("select distinct t1.awardAttachmentId from AwardAttachment t1 inner join Award t2 on t1.awardNumber = t2.awardNumber ")
					.append("where t1.documentId not in (SELECT documentId FROM AwardAttachment where awardNumber =: awardNumber ")
					.append("and documentStatusCode = 3 and sequenceNumber <= ")
					.append("(select max(sequenceNumber) from Award where awardNumber =: awardNumber AND awardSequenceStatus = 'ARCHIVE') ")
					.append(") and t1.awardNumber =: awardNumber and t1.sequenceNumber <=  ")
					.append("(select max(sequenceNumber) from Award where awardNumber =: awardNumber AND awardSequenceStatus = 'ARCHIVE') )")
					.append("and (t4.awardSequenceStatus = 'ARCHIVE' or t3.awardId =: awardId)")
					.append(filterCondition) + ")"
					.toString();
			org.hibernate.query.Query<AwardAttachment>  query  = session.createQuery(attachmentQuery);
			query.setParameter(AWARDNUMBER, awardNumber);
			query.setParameter("awardId", awardId);
			return query.getResultList();
		}
	}

	@Override
	public Award fetchAwardSetUpByAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		Predicate predicateThree = builder.equal(rootAward.get("awardDocumentTypeCode"), Constants.AWARD_SETUP);
		query.where(builder.and(predicateOne, predicateThree));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public void createOrUpdateMasterAward(Integer awardId, String updateUser) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call MERGE_AWARD_CONCURRENT_DATA(?,?)}");
				statement.setInt(1, awardId);
				statement.setString(2, updateUser);
				statement.execute();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "MERGE_AWARD_CONCURRENT_DATA";
				String functionCall = "{call " + procedureName + "(?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, awardId);
				statement.setString(3, updateUser);
				statement.execute();
			}
		} catch (SQLException e) {
			logger.error("Exception in Create or update Master Award: {} ", e.getMessage());
			throw new ApplicationException("error occured in createOrUpdateMasterAward", e, Constants.DB_PROC_ERROR);
		}
	}

	@Override
	public Integer getLatestArchiveAwardId(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> outerQuery = builder.createQuery(Integer.class);
		Root<Award> rootAward = outerQuery.from(Award.class);
		Subquery<Integer> subQuery = outerQuery.subquery(Integer.class);
		Root<Award> subRootAward = subQuery.from(Award.class);
		try {
			Predicate predicateAwardNumber = builder.equal(subRootAward.get(AWARDNUMBER), awardNumber);
			Predicate predicateAwardSequenceStatus = builder.equal(subRootAward.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ARCHIVE);
			subQuery.select(builder.max(subRootAward.get(UPDATETIMESTAMP))).where(builder.and(predicateAwardNumber, predicateAwardSequenceStatus));
			outerQuery.select(rootAward.get(AWARDID));
			outerQuery.where(builder.and(builder.in(rootAward.get(UPDATETIMESTAMP)).value(subQuery),
					builder.equal(rootAward.get(AWARDNUMBER), awardNumber),
					builder.equal(rootAward.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ARCHIVE)));
			return session.createQuery(outerQuery).uniqueResult();
		} catch (Exception e) {
			try {
				return session.createQuery(outerQuery).getResultList().get(0);
			} catch (Exception ex) {
				throw new ApplicationException("error occured in getLatestArchiveAwardId ", ex, Constants.JAVA_ERROR);
			}
		}
	}

	@Override
	public void saveOrUpdateAwardHistoryLog(AwardHistoryLog awardHistoryLog) {
		try {
			hibernateTemplate.saveOrUpdate(awardHistoryLog);
		} catch (Exception e) {
			e.printStackTrace();
			throw new ApplicationException("Error occurred in saveOrUpdateAwardHistoryLog", e, Constants.JAVA_ERROR);
		}
	}

	@Override
	public AwardHistoryLog getAwardHistoryLogBasedOnAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardHistoryLog> query = builder.createQuery(AwardHistoryLog.class);
		Root<AwardHistoryLog> rootAward = query.from(AwardHistoryLog.class);
		query.where(builder.equal(rootAward.get("awardId"), awardId));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public String getAwardSequenceStatusByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT awardSequenceStatus FROM Award WHERE awardId=:awardId";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		return (String) query.getSingleResult();
	}

	@Override
	public void updateAwardDocumentUpdateUserAndTimestamp(Integer awardId) {																											  
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<Award> criteriaUpdate = cb.createCriteriaUpdate(Award.class);
		Root<Award> awardRoot = criteriaUpdate.from(Award.class);
		criteriaUpdate.set("documentUpdateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.set("documentUpdateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.where(cb.equal(awardRoot.get("awardId"),awardId)); 																			 
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@Override
	public Boolean checkPersonHasRightInAward(String personId, String rightName, Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String approverPersonId = new StringBuilder("FROM AwardPersonRoles T1 ")
				.append(" INNER JOIN RoleRights T2 ON T1.roleId=T2.roleId ")
				.append(" INNER JOIN Rights T3 ON T3.rightId = T2.rightId where T1.awardId =:awardId and T1.personId =:personId ")
				.append(" and T3.rightName =:rightName").toString();
		Query query = session.createQuery(approverPersonId);
		query.setParameter("personId", personId);
		query.setParameter("rightName", rightName);
		query.setParameter("awardId", awardId);
		return !query.getResultList().isEmpty();
	}

	@Override
	public String canCreateVariationRequest(Integer awardId, String variationTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		String result = "";
		try {
			statement = connection.prepareCall("{ ? = call FN_CHK_CAN_CREATE_VARIATION(?,?) }");
			statement.registerOutParameter(1, OracleTypes.INTEGER);
			statement.setInt(2, awardId);
			statement.setString(3, variationTypeCode);
			statement.execute();
			result = statement.getString(1);
			return result;
		} catch (SQLException e) {
			logger.error("exception in canCreateVariationRequest: {} ", e.getMessage());
		}
		return result;
	}

	@Override
	public AwardAttachment getAwardAttachmentDetailsBasedOnParams(String awardNumber, String fileName, String letterOfAcceptance) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAttachment> outerQuery = builder.createQuery(AwardAttachment.class);
		Root<AwardAttachment> rootAwardAttachment = outerQuery.from(AwardAttachment.class);
		Subquery<Integer> subQuery = outerQuery.subquery(Integer.class);
		Root<AwardAttachment> subRootAwardAttachment = subQuery.from(AwardAttachment.class);
		Predicate predicateAwardNumber = builder.equal(subRootAwardAttachment.get(AWARDNUMBER), awardNumber);
		Predicate predicateFileName = builder.equal(subRootAwardAttachment.get(FILE_NAME), fileName);
		Predicate predicateAttachmentType = builder.equal(subRootAwardAttachment.get("typeCode"), letterOfAcceptance);
		subQuery.select(builder.max(subRootAwardAttachment.get("awardAttachmentId"))).where(builder.and(predicateAwardNumber, predicateFileName, predicateAttachmentType));
		outerQuery.where(builder.in(rootAwardAttachment.get("awardAttachmentId")).value(subQuery));
		return session.createQuery(outerQuery).uniqueResult();
	}

	@Override
	public Boolean isDatesAndAmountEditable(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ModuleVariableSection> outerQuery = builder.createQuery(ModuleVariableSection.class);
		Root<ModuleVariableSection> rootModuleVariableSection = outerQuery.from(ModuleVariableSection.class);
		Predicate predicateAwardId = builder.equal(rootModuleVariableSection.get("moduleItemKey"), awardId);
		Predicate predicateModuleCode = builder.equal(rootModuleVariableSection.get("moduleCode"), Constants.AWARD_MODULE_CODE);
		Predicate predicateSubModuleCode = builder.equal(rootModuleVariableSection.get("subModuleCode"), Constants.AWARD_SUBMODULE_CODE);
		Predicate predicateSectionCode = builder.equal(rootModuleVariableSection.get("sectionCode"), Constants.DATES_AND_AMOUNT_EDITABLE_SECTION_TYPE_CODE);
		outerQuery.where(builder.and(predicateAwardId, predicateModuleCode, predicateSubModuleCode, predicateSectionCode));
		return session.createQuery(outerQuery).uniqueResult() != null ? Boolean.TRUE : Boolean.FALSE;
	}

	@Override
	public String getAwardDocumentTypeCode(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateAwardId = builder.equal(rootAward.get("awardId"), awardId);
		query.select(rootAward.get("awardDocumentTypeCode"));
		query.where(builder.and(predicateAwardId));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public List<Integer> fetchAllExpiringAwardIds() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> outerQuery = builder.createQuery(Integer.class);
		Root<Award> rootAward = outerQuery.from(Award.class);
		Predicate predicate1 = builder.lessThanOrEqualTo(rootAward.get("finalExpirationDate"), commonDao.getStartTimeOfCurrentDay());
		Predicate predicate2 = builder.equal(rootAward.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ACTIVE);
		Predicate predicate3 = builder.not(rootAward.get("statusCode").in(Constants.AWARD_STATUS_CODE_EXPIRED, Constants.AWARD_STATUS_CODE_CLOSED));
		outerQuery.select(rootAward.get("awardId"));
		outerQuery.where(builder.and(predicate1, predicate2, predicate3));
		return session.createQuery(outerQuery).getResultList();
	}

	@Override
	public void updateAwardStatusToExpired(List<Integer> awardIds) {																											  
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<Award> criteriaUpdate = cb.createCriteriaUpdate(Award.class);
		Root<Award> awardRoot = criteriaUpdate.from(Award.class);
		criteriaUpdate.set("documentUpdateTimeStamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set(UPDATETIMESTAMP, commonDao.getCurrentTimestamp());
		criteriaUpdate.set("statusCode", Constants.AWARD_STATUS_CODE_EXPIRED);
		criteriaUpdate.where(awardRoot.get("awardId").in(awardIds)); 																			 
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@Override
	public AwardDocumentType getAwardDocumentTypeByAwardId(Integer awardId) {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append(" FROM AwardDocumentType where awardDocumentTypeCode = (select awardDocumentTypeCode from  Award WHERE awardId = :awardId)");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			query.setParameter("awardId", awardId);
			return (AwardDocumentType) query.getSingleResult();
		} catch (Exception e) {
			logger.error("Exception in getAwardDocumentTypeByAwardId : {}", e.getMessage());
			return null;
		}
	}
	
  @Override
	public void archiveOldAttachmentVersion(Integer awardReportTrackingId, int versionNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<AwardReportTrackingFile> criteriaUpdate = cb.createCriteriaUpdate(AwardReportTrackingFile.class);
		Root<AwardReportTrackingFile> root = criteriaUpdate.from(AwardReportTrackingFile.class);
		criteriaUpdate.set("documentStatusCode", "2");
		Predicate predicateId = cb.equal(root.get("awardReportTrackingId"), awardReportTrackingId);
		Predicate predicateVersion = cb.equal(root.get("versionNumber") ,versionNumber);
		criteriaUpdate.where(cb.and(predicateId, predicateVersion));
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public List<AwardReportTrackingFile> getReportTrackingAttachmentVersions(Integer awardReportTrackingId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardReportTrackingFile> criteria = builder.createQuery(AwardReportTrackingFile.class);
			Root<AwardReportTrackingFile> root = criteria.from(AwardReportTrackingFile.class);
			Predicate predicateId = builder.equal(root.get("awardReportTrackingId"),awardReportTrackingId);
			Predicate predicateStatusCode = builder.equal(root.get("documentStatusCode"),"2");
			criteria.where(builder.and(predicateId, predicateStatusCode));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public List<AwardReportTrackingFile> getAllDraftReportTrackingAttachments(List<Integer> trackingId) {
		return hibernateTemplate.execute(session -> {
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AwardReportTrackingFile> criteria = builder.createQuery(AwardReportTrackingFile.class);
			Root<AwardReportTrackingFile> root = criteria.from(AwardReportTrackingFile.class);
			Expression<String> codes = root.get("awardReportTrackingId");
			Predicate predicateId = codes.in(trackingId);
			Predicate predicateStatusCode = builder.equal(root.get("documentStatusCode"),"1");
			criteria.where(builder.and(predicateId, predicateStatusCode));
			return session.createQuery(criteria).getResultList();
		});
	}

	@Override
	public AwardReportTracking saveOrUpdateReportTracking(AwardReportTracking awardReportTracking) {
		hibernateTemplate.saveOrUpdate(awardReportTracking);
		return awardReportTracking;
	}

	@Override
	public void deleteReportTracking(Integer awardReportTrackingId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaDelete<AwardReportTracking> criteriaDelete = cb.createCriteriaDelete(AwardReportTracking.class);
		Root<AwardReportTracking> root = criteriaDelete.from(AwardReportTracking.class);
		criteriaDelete.where(cb.equal(root.get("awardReportTrackingId"), awardReportTrackingId));
		session.createQuery(criteriaDelete).executeUpdate();
	}

	@Override
	public Boolean getIsFileDataIdFound(String fileId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<Long> query = cb.createQuery(Long.class);
			Root<AwardReportTrackingFile> root = query.from(AwardReportTrackingFile.class);
			query.select(cb.count(root));
			query.where(cb.equal(root.get("fileId"), fileId));
			Long count = session.createQuery(query).getSingleResult();
			return count.intValue() > 1 ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.TRUE;
		}
	}

	@Override
	public void deleteAwardReportTrackingFileByTrackingId(List<Integer> awardReportTrackingIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaDelete<AwardReportTrackingFile> criteriaDelete = cb.createCriteriaDelete(AwardReportTrackingFile.class);
		Root<AwardReportTrackingFile> root = criteriaDelete.from(AwardReportTrackingFile.class);
		criteriaDelete.where(root.get("awardReportTrackingId").in(awardReportTrackingIds));
		session.createQuery(criteriaDelete).executeUpdate();
	
	}

	@SuppressWarnings("unchecked")
	@Override
	public void deleteReportTrackingAttachment(Integer awardReportTrackingFileId, Integer reportTrackingId,
			Integer reportTermsId) {
		try {
			Query queryFile = null;
			Query query = null;
			List<String> fileIds = null;
			if (awardReportTrackingFileId != null) {
				queryFile = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
						"select fileId from AwardReportTrackingFile where awardReportTrackingFileId = :awardReportTrackingFileId ");
				queryFile.setParameter("awardReportTrackingFileId",awardReportTrackingFileId);
				fileIds = queryFile.getResultList();
				deleteFileData(fileIds);
				query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
						"delete from AwardReportTrackingFile where awardReportTrackingFileId = :awardReportTrackingFileId ");
				query.setParameter("awardReportTrackingFileId",awardReportTrackingFileId);
			} else if (reportTermsId != null) {
				queryFile = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
						"select fileId  from AwardReportTrackingFile where awardReportTermsId = :awardReportTermsId ");
				queryFile.setParameter("awardReportTermsId",reportTermsId);
				fileIds = queryFile.getResultList();
				deleteFileData(fileIds);
				query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
						"delete from AwardReportTrackingFile where awardReportTermsId = :awardReportTermsId ");
				query.setParameter("awardReportTermsId",reportTermsId);
			} else if (reportTrackingId != null) {
				queryFile = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
						"select fileId from AwardReportTrackingFile where awardReportTrackingId = :awardReportTrackingId ");
				queryFile.setParameter("awardReportTrackingId",reportTrackingId);
				fileIds = queryFile.getResultList();
				deleteFileData(fileIds);
				query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
						"delete from AwardReportTrackingFile where awardReportTrackingId = :awardReportTrackingId ");
				query.setParameter("awardReportTrackingId",reportTrackingId);
			}
			query.executeUpdate();
		} catch (Exception e) {
			logger.error("Exception in deleteReportTrackingAttachment : {}", e.getMessage());
		}
		
	}

	private void deleteFileData(List<String> fileIds) {
		fileIds.forEach(file -> {
			if (Boolean.FALSE.equals(getIsFileDataIdFound(file))) {
				commonDao.deleteFileData(commonDao.getFileDataById(file));
			}
		});		
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Integer> getListOfPendingAwardForAwardNumber(String awardNumber) {
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
				"select awardId from Award where awardNumber =:awardNumber and awardSequenceStatus = 'PENDING' ");
		query.setParameter("awardNumber",awardNumber);
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<AwardReportTracking> getAwardReportTrackingByParameters(Integer awardId,
			String frequencyBaseProjectEndDate) {
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
				"select t1 from AwardReportTracking t1 inner join AwardReportTerms t2 on t1.awardReportTerms.awardReportTermsId = t2.awardReportTermsId where t1.awardId =:awardId and t2.frequencyBaseCode =:frequencyBaseProjectEndDate ");
		query.setParameter("awardId",awardId);
		query.setParameter("frequencyBaseProjectEndDate",frequencyBaseProjectEndDate);
		return query.getResultList();
	}

	@Override
	public List<Award> fetchActiveAwardByAwardNumbers(List<String> awardNumbers) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();						
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> criteria = builder.createQuery(Award.class);
		Root<Award> root = criteria.from(Award.class);
		Predicate predicateAwardNumbers = root.get(AWARDNUMBER).in(awardNumbers);
		Predicate predicateAwardSequenceStatus = builder.equal(root.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ACTIVE);
		criteria.where(builder.and(predicateAwardNumbers, predicateAwardSequenceStatus));
		return session.createQuery(criteria).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public Set<AwardSearchResult> findAward(String searchString) {
		try {
			searchString = new StringBuilder("%").append(searchString).append("%").toString();
			String hqlQuery = new StringBuilder("Select distinct new com.polus.fibicomp.award.dto.AwardSearchResult( T1,T6.sponsorName,T7.unitName)from Award T1 ")
							.append("left join AwardPerson T2 on T1.awardId = T2.awardId ")
							.append("left join AwardPersonRoles T3 on T1.awardId = T3.awardId ")
							.append("left join RoleRights T4 on T4.roleId = T3.roleId ")
							.append("left join Rights T5 on T5.rightId = T4.rightId ")
							.append("inner join Sponsor T6 ON T6.sponsorCode = T1.sponsorCode ")
							.append("inner join Unit T7 ON T7.unitNumber = T1.leadUnitNumber ")
							.append("left join PersonRoleRT T8 ON T8.personRoleRTAttributes.personId = :personId AND T8.personRoleRTAttributes.unitNumber = T1.leadUnitNumber")
							.append(" where(T1.title like :searchCriteria or T1.awardId like :searchCriteria or T1.awardNumber like :searchCriteria or ")
							.append("T1.accountNumber like :searchCriteria or T1.sponsorAwardNumber like :searchCriteria or ")
							.append("(T2.fullName like :searchCriteria and T2.personRoleId =3) or T6.sponsorName like :searchCriteria or T7.unitName like :searchCriteria ")
							.append(") and ((T5.rightName ='CREATE_PROGRESS_REPORT' and T3.personId =: personId) or (T8.personRoleRTAttributes.rightName = 'CREATE_PROGRESS_REPORT')) and T1.awardSequenceStatus = 'ACTIVE'").toString();
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery);
			query.setParameter("searchCriteria", searchString);
			query.setParameter("personId", AuthenticatedUser.getLoginPersonId());	
			return new HashSet<>(query.setMaxResults(25).getResultList());
		} catch (Exception e) {
			logger.error("Error in findAward : {}", e.getMessage());
			return new HashSet<>();
		}
	}
	
	@Override
	public Boolean checkAwardReportTermsExist(Integer awardId) {
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
				"select count(1) from AwardReportTerms where awardId =:awardId ");
		query.setParameter("awardId",awardId);
		return (Long)query.getSingleResult() > 0 ? Boolean.TRUE : Boolean.FALSE;
	}

	@Override
	public void deleteAwardReportByAwardId(Integer awardId) {		
		boolean isProgressReportEnabled = commonDao.getParameterValueAsBoolean(Constants.AWARD_PROGRESS_REPORT_ENABLED);
		deleteReportTermsForPR(isProgressReportEnabled, awardId);
		deleteReportTermsForFiles(isProgressReportEnabled, awardId);				
	}

	@SuppressWarnings("unchecked")
	private void deleteReportTermsForPR(boolean isProgressReportEnabled, Integer awardId) {
		if(isProgressReportEnabled) {
			Query queryProgressReport = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"select awardReportTerms.awardReportTermsId from AwardReportTracking where awardId =:awardId and progressReportId is null and awardReportTerms.reportClassCode in (1,2)");
			queryProgressReport.setParameter("awardId",awardId);		
			List<Integer> awardReportPRTermIds = queryProgressReport.getResultList();
			if(!awardReportPRTermIds.isEmpty()) {
				Query queryDeleteProgressReport = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
						"delete from AwardReportTracking where awardReportTerms.awardReportTermsId in :awardReportPRTermIds ");
				queryDeleteProgressReport.setParameter("awardReportPRTermIds",awardReportPRTermIds);
				queryDeleteProgressReport.executeUpdate();
				
				Query queryDeleteProgressReportRec = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
						"delete from AwardReportTermRecipient where awardReportTerms.awardReportTermsId  in :awardReportPRTermIds ");
				queryDeleteProgressReportRec.setParameter("awardReportPRTermIds",awardReportPRTermIds);
				queryDeleteProgressReportRec.executeUpdate();
				
				Query queryDeleteProgressReportTerms = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
						"delete from AwardReportTerms where awardReportTermsId  in :awardReportPRTermIds ");
				queryDeleteProgressReportTerms.setParameter("awardReportPRTermIds",awardReportPRTermIds);
				queryDeleteProgressReportTerms.executeUpdate();
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void deleteReportTermsForFiles(boolean isProgressReportEnabled, Integer awardId) {
		Query queryAttachment = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
				"select t1.awardReportTermsId from AwardReportTerms t1 left join AwardReportTrackingFile t2 on t2.awardReportTermsId = t1.awardReportTermsId where t1.awardId =:awardId and t2.fileId = null and t1.reportClassCode not in (1,2) ");
		queryAttachment.setParameter("awardId",awardId);
		if(!isProgressReportEnabled) {
			queryAttachment = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"select t1.awardReportTermsId from AwardReportTerms t1 left join AwardReportTrackingFile t2 on t2.awardReportTermsId = t1.awardReportTermsId where t1.awardId =:awardId and t2.fileId = null ");
			queryAttachment.setParameter("awardId",awardId);
		}		
		List<Integer> awardReportAttachmentTermsId = queryAttachment.getResultList();		
		if(!awardReportAttachmentTermsId.isEmpty()) {
			Query queryDeleteAttachFiles = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"delete from AwardReportTrackingFile where awardReportTermsId in :awardReportAttachmentTermsId ");
			queryDeleteAttachFiles.setParameter("awardReportAttachmentTermsId",awardReportAttachmentTermsId);
			queryDeleteAttachFiles.executeUpdate();
			
			Query queryDeleteAttach = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"delete from AwardReportTracking where awardReportTerms.awardReportTermsId in :awardReportAttachmentTermsId ");
			queryDeleteAttach.setParameter("awardReportAttachmentTermsId",awardReportAttachmentTermsId);
			queryDeleteAttach.executeUpdate();
			
			Query queryDeleteAttachRec = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"delete from AwardReportTermRecipient where awardReportTerms.awardReportTermsId in :awardReportAttachmentTermsId ");
			queryDeleteAttachRec.setParameter("awardReportAttachmentTermsId",awardReportAttachmentTermsId);
			queryDeleteAttachRec.executeUpdate();

			Query queryDeleteAttachTerms = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
					"delete from AwardReportTerms where awardReportTermsId in :awardReportAttachmentTermsId ");
			queryDeleteAttachTerms.setParameter("awardReportAttachmentTermsId",awardReportAttachmentTermsId);
			queryDeleteAttachTerms.executeUpdate();
		}		
	}

	@Override
	public void deleteAwardSponsorTermsByAwardId(Integer awardId) {
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
				"delete from AwardSponsorTerm where awardId =:awardId ");
		query.setParameter("awardId",awardId);
	    query.executeUpdate();		
	}

	@Override
	public Boolean checkAwardSponsorTermsExist(Integer awardId) {
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
				"select count(1) from AwardSponsorTerm where awardId =:awardId ");
		query.setParameter("awardId",awardId);
		return (Long)query.getSingleResult() > 0 ? Boolean.TRUE : Boolean.FALSE;
  }
  
	public Integer fetchActiveAwardIdByAwardNumber(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
		Predicate predicateThree = builder.equal(rootAward.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ACTIVE);
		query.select(rootAward.get("awardId"));
		query.where(builder.and(predicateOne, predicateThree));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public SapFeedStatus fetchFeedStatusBasedOnAward(Integer awardId, String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		SapFeedStatus sapFeedStatus = new SapFeedStatus();
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_SAP_FEED_STATUS(?,?)}");
				statement.setInt(1, awardId);
				statement.setString(2, awardNumber);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_SAP_FEED_STATUS";
				String functionCall = "{call " + procedureName + "(?,?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, awardId);
				statement.setString(3, awardNumber);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset != null && rset.next()) {
				sapFeedStatus.setFeedStatusCode(rset.getString("FEED_STATUS_CODE"));
				sapFeedStatus.setDescription(rset.getString(DESC));
			}
		} catch (Exception e) {
			e.printStackTrace();
			return sapFeedStatus;
		}
		return sapFeedStatus;
	}

	@Override
	public void updateAwardStatusByStatusCode(Integer awardId, String statusCode) {																											  
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<Award> criteriaUpdate = cb.createCriteriaUpdate(Award.class);
		Root<Award> rootAward = criteriaUpdate.from(Award.class);
		criteriaUpdate.set("statusCode",statusCode);
		Predicate predicateAwardId = cb.equal(rootAward.get("awardId"), awardId);
		criteriaUpdate.where(cb.and(predicateAwardId)); 																			 
		session.createQuery(criteriaUpdate).executeUpdate();		
	}

	@Override
	public String fetchActiveAwardLeadUnitByAwardNumber(String awardNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<String> query = builder.createQuery(String.class);
			Root<Award> rootAward = query.from(Award.class);
			Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
			Predicate predicateThree = builder.equal(rootAward.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ACTIVE);
			query.select(rootAward.get("leadUnitNumber"));
			query.where(builder.and(predicateOne, predicateThree));
			return session.createQuery(query).uniqueResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<String> getAwardVariationsBasedOnAwardNumber(String awardNumber) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<String> query = builder.createQuery(String.class);
			Root<Award> rootAward = query.from(Award.class);
			Predicate predicateOne = builder.equal(rootAward.get(AWARDNUMBER), awardNumber);
			Predicate predicateThree = builder.equal(rootAward.get(AWARDSEQUENCESTATUS), Constants.AWARD_FINAL_STATUS_ARCHIVE);
			query.select(rootAward.get("awardVariationTypeCode"));
			query.where(builder.and(predicateOne, predicateThree));
			return session.createQuery(query).getResultList();
		} catch (Exception e) {
			return new ArrayList<>();
		}
	}

	@Override
	public String getAwardActivityTypeCodeByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT activityTypeCode FROM Award WHERE awardId=:awardId";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		return (String) query.getSingleResult();
	}

	@Override
	public Award getAwardSequenceNumberAndSeqStatusByAwardId(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT new Award(sequenceNumber, awardSequenceStatus) FROM Award WHERE awardId=:awardId";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("awardId", awardId);
		return (Award) query.getSingleResult();
	}

	@Override
	public String getAwardCreateUser(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT t2.personId FROM Award t1 inner join Person t2 on t1.createUser = t2.principalName  WHERE t1.awardNumber=:awardNumber and t1.awardDocumentTypeCode = 1";
		javax.persistence.Query query = session.createQuery(hqlQuery);
		query.setParameter("awardNumber", awardNumber);
		return (String) query.getSingleResult();
	}

	@Override
	public void deleteAwardByAwardID(Integer awardID) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			ProcedureCall procedure = session.createStoredProcedureCall("DELETE_AWARD_BY_ID");
			procedure.registerParameter(1, Integer.class, ParameterMode.IN).bindValue(awardID);
			procedure.execute();
		} catch (Exception e) {
			logger.error("Error occured while deleteAwardByID");
		}
		
	}

	@Override
	public Integer checkChildAwardExisted(String awardNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "select count(1) from AwardHierarchy where rootAwardNumber=:awardNumber";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("awardNumber", awardNumber);
		return Integer.parseInt(query.getSingleResult().toString());
	}

	@Override
	public List<MilestoneStatus> getAwardMilestoneStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<MilestoneStatus> query = builder.createQuery(MilestoneStatus.class);
		Root<MilestoneStatus> rootAgreementModule = query.from(MilestoneStatus.class);
		query.where(builder.equal(rootAgreementModule.get("isActive"), true));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteAwardReportTrackingAfterEndDate(Integer awardId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			ProcedureCall procedure = session.createStoredProcedureCall("DEL_DUE_DATE_AFTER_END_DATE");
			procedure.registerParameter(1, Integer.class, ParameterMode.IN).bindValue(awardId);
			procedure.execute();
		} catch (Exception e) {
			logger.error("Exception in Delete Award Report Tracking After Award EndDate : {}", e.getMessage());
			throw new ApplicationException("Error occurred in deleteAwardReportTrackingAfterEndDate", e, Constants.DB_PROC_ERROR);
		}
	}

	@Override
	public Map<String, Timestamp> getAwardBaseDates(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		Map<String, Timestamp> mapOfDates = new HashMap<>();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet rset = null;
		try {
			if (oracledb.equalsIgnoreCase("N")) {
				statement = connection.prepareCall("{call GET_AWARD_REPORT_REQ_BASE_DATES(?)}");
				statement.setInt(1, awardId);
				statement.execute();
				rset = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase("Y")) {
				String procedureName = "GET_AWARD_REPORT_REQ_BASE_DATES";
				String functionCall = "{call " + procedureName + "(?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.registerOutParameter(1, OracleTypes.CURSOR);
				statement.setInt(2, awardId);
				statement.execute();
				rset = (ResultSet) statement.getObject(1);
			}
			while (rset != null && rset.next()) {
				String baseDate = rset.getString("BASE_DATE");
				if (baseDate != null && !baseDate.isEmpty()) {
					mapOfDates.put(rset.getString("FREQUENCY_BASE_CODE"),  Timestamp.valueOf(baseDate));
				}
			}
		} catch (SQLException e) {
			logger.error("Exception in getAwardBaseDates: {} ", e.getMessage());
		} finally {
			try {
				if (statement != null) {
					statement.close();
				}
			} catch (Exception e) {
				logger.error("Exception {}", e.getMessage());
			}
		}
		return mapOfDates;
	}

	@Override
	public Boolean fetchBaseDateIncludeStatus(String code) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "select includeBaseDate from FrequencyBase where frequencyBaseCode=:code";
			Query query = session.createQuery(hqlQuery);
			query.setParameter("code", code);
			Boolean includeBaseDate = (Boolean) query.getSingleResult();
			return includeBaseDate != null ? includeBaseDate : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.FALSE;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Award> getAwardHistoryDetails(String awardNumber) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT NEW com.polus.fibicomp.award.pojo.Award(T1.awardId, T1.awardNumber, T1.sequenceNumber, ");
		hqlQuery.append("T1.leadUnitNumber, T1.leadUnit, T1.title, T1.statusCode, T1.awardStatus, T1.awardSequenceStatus, ");
		hqlQuery.append("T7, T1.submissionDate, T4.fullName, T5.fullName, T6.fullName, ");
		hqlQuery.append("T3.fullName, T1.updateTimeStamp, T1.createTimestamp) ");
		hqlQuery.append("from Award T1 ");
		hqlQuery.append("left join AwardPerson T3 on T3.awardId = T1.awardId and T3.isPi = true ");
		hqlQuery.append("left join Person T4 on T4.principalName = T1.submitUser ");
		hqlQuery.append("left join Person T5 on T5.principalName = T1.createUser ");
		hqlQuery.append("left join Person T6 on T6.principalName = T1.updateUser ");
		hqlQuery.append("left join ServiceRequestType T7 on T7.typeCode = T1.awardVariationTypeCode ");
		hqlQuery.append("where T1.awardNumber = : awardNumber order by T1.sequenceNumber desc ");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("awardNumber", awardNumber);
		return query.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<ServiceRequest> getServiceRequestDetailsBasedOnModuleCodeAndOriginatingModuleItemKeys(Integer moduleCode, List<String> originatingModuleItemKeys) {
		StringBuilder hqlQuery = new StringBuilder();
		hqlQuery.append("SELECT NEW com.polus.fibicomp.servicerequest.pojo.ServiceRequest(T1.serviceRequestId, T1.subject, T1.description, T1.originatingModuleItemKey) ");
		hqlQuery.append("from ServiceRequest T1 where T1.originatingModuleItemKey in :originatingModuleItemKey and T1.moduleCode = : moduleCode and T1.isSystemGenerated = true ");
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		query.setParameter("originatingModuleItemKey", originatingModuleItemKeys);
		query.setParameter("moduleCode", moduleCode);
		return query.getResultList();
	}

}
