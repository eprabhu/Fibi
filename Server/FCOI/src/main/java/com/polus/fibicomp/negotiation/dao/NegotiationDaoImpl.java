package com.polus.fibicomp.negotiation.dao;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.TimeUnit;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.apache.commons.collections4.ListUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Criteria;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.hibernate.internal.SessionImpl;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.correspondence.dao.DocxDocumentMergerAndConverter;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.negotiation.dto.NegotiationDataBus;
import com.polus.fibicomp.negotiation.dto.NegotiationProjectDetailsDto;
import com.polus.fibicomp.negotiation.dto.NegotiationReportDto;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivity;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAgreementValue;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationDetails;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocationType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsPersonnel;
import com.polus.fibicomp.negotiation.vo.LastLocationDetails;
import com.polus.fibicomp.negotiation.vo.NegotiationMode;
import com.polus.fibicomp.negotiation.vo.NegotiationVO;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.utils.QueryBuilder;
import com.polus.fibicomp.vo.OrganizationSearchResult;
import com.polus.fibicomp.vo.SponsorSearchResult;

import fr.opensagres.xdocreport.document.IXDocReport;
import fr.opensagres.xdocreport.document.registry.XDocReportRegistry;
import fr.opensagres.xdocreport.template.IContext;
import fr.opensagres.xdocreport.template.TemplateEngineKind;
import oracle.jdbc.OracleTypes;

@Transactional
@Service(value = "negotiationDao")
public class NegotiationDaoImpl implements NegotiationDao {

	protected static Logger logger = LogManager.getLogger(NegotiationDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;
	
	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private PersonDao personDao;

	private static final String NEGOTIATION_ID = "negotiationId";
	private static final String NEGOTIATION_LOCATION_ID = "negotiationLocationId";
	private static final String LOCATION_TYPE_CODE = "locationTypeCode";

	@Override
	public String addNegotiationActivity(NegotiationsActivity negotiationActivity) {
		if (negotiationActivity != null) {
			hibernateTemplate.save(negotiationActivity);
			return commonDao.convertObjectToJSON("Actvity is added successfully");
		}
		return commonDao.convertObjectToJSON("");
	}

	@Override
	public NegotiationVO loadNegotiation(NegotiationVO negotiationVO) {
		NegotiationMode negotiationsMode = new NegotiationMode();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("deprecation")
		Criteria criteria = session.createCriteria(Negotiations.class);
		criteria.add(Restrictions.like(NEGOTIATION_ID, negotiationVO.getNegotiationId()));
		@SuppressWarnings("unchecked")
		List<Negotiations> negotiationList = criteria.list();
		if (negotiationList != null &&  !negotiationList.isEmpty()) {
			Negotiations negotiation = negotiationList.get(0);
			loadNegotiationUserFullNames(negotiation);
			negotiationVO.setNegotiations(negotiation);
			negotiationsMode = getNegotiationModeData(negotiation);
			if (negotiationsMode.getMode().equalsIgnoreCase(Constants.NEGOTIATION_EDIT_MODE)) {
				negotiationVO.setIsSubmit(Constants.NEGOTIATION_SHOW_SUBMIT_BUTTON);
			}
		}
		negotiationVO.setNegotiationMode(negotiationsMode);
		String organizationName = getSubAwardOrganizationName(negotiationVO.getNegotiationId());
		negotiationVO.setSubAwardOrganizationName(organizationName);
		return negotiationVO;
	}

	@SuppressWarnings("unused")
	private String getsponsorName(int negotiationId) {
		String sponsorName = "";
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("deprecation")
		Criteria criteria = session.createCriteria(NegotiationsAssociationDetails.class);
		criteria.add(Restrictions.like(NEGOTIATION_ID, negotiationId));
		@SuppressWarnings("unchecked")
		List<NegotiationsAssociationDetails> negotiationAssociationList = criteria.list();
		if (negotiationAssociationList != null && !negotiationAssociationList.isEmpty()) {
			NegotiationsAssociationDetails negotiationsAssociationDetails = negotiationAssociationList.get(0);
			String sponsorCode = negotiationsAssociationDetails.getSponsorCode();
			sponsorName = getsponsorNameByCode(sponsorCode);
		}
		return sponsorName;
	}

	private String getsponsorNameByCode(String sponsorCode) {
		String sponsorName = "";
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<Sponsor> rootNegotiationsLocation = query.from(Sponsor.class);
		query.where(builder.equal(rootNegotiationsLocation.get("sponsorCode"), sponsorCode));
		query.multiselect(builder.max(rootNegotiationsLocation.get("sponsorName")));
		if (session.createQuery(query).uniqueResult() != null) {
			sponsorName = session.createQuery(query).uniqueResult();
			logger.info("projections value : {}", sponsorName);
		}
		return sponsorName;
	}

	private String getOrgnizationById(String subAwardId) {
		String organizationName = "";
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<Organization> rootNegotiationsLocation = query.from(Organization.class);
		query.where(builder.equal(rootNegotiationsLocation.get("organizationId"), subAwardId));
		query.multiselect(builder.max(rootNegotiationsLocation.get("organizationName")));
		if (session.createQuery(query).uniqueResult() != null) {
			organizationName = session.createQuery(query).uniqueResult();
			logger.info("projections value : {}", organizationName);
		}
		return organizationName;
	}

	private NegotiationMode getNegotiationModeData(Negotiations negotiation) {
		NegotiationMode negotiationMode = new NegotiationMode();
		if (negotiation.getNegotiationStatusCode().equalsIgnoreCase(Constants.NEGOTIATION_STATUS_CODE_APPROVAL_INPROGRESS.toString())
				|| negotiation.getNegotiationStatusCode()
						.equalsIgnoreCase(Constants.NEGOTIATION_MODE_WAITING_FOR_RESPONSE)) {
			negotiationMode.setMode("VIEW");
		} else {
			negotiationMode.setMode("EDIT");
		}
		negotiationMode.setStatus(negotiation.getNegotiationStatusCode());
		return negotiationMode;
	}

	@Override
	public NegotiationVO getNegotiationsLocationHistory(Integer negotiationId) {
		NegotiationVO negotiationVO = new NegotiationVO();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		List<HashMap<String, Object>> locationHistoryList = new ArrayList<HashMap<String, Object>>();

		try {
			if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_NEGO_LOCATION_HISTORY(?)}");
				statement.setInt(1, negotiationId);
				statement.execute();
				resultSet = statement.getResultSet();
			} else if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "GET_NEGO_LOCATION_HISTORY";
				String functionCall = "{call " + procedureName + "(?,?)}";
				statement = connection.prepareCall(functionCall);
				statement.setInt(1, negotiationId);
				statement.registerOutParameter(2, OracleTypes.CURSOR);
				statement.execute();
				resultSet = (ResultSet) statement.getObject(2);
			}
			if (resultSet != null) {
				while (resultSet.next()) {
					HashMap<String, Object> locationDetail = new HashMap<String, Object>();
					locationDetail.put(NEGOTIATION_LOCATION_ID, resultSet.getString("NEGOTIATION_LOCATION_ID"));
					locationDetail.put(NEGOTIATION_ID, resultSet.getString("NEGOTIATION_ID"));
					locationDetail.put(LOCATION_TYPE_CODE, resultSet.getString("LOCATION_TYPE_CODE"));
					locationDetail.put("description", resultSet.getString("LOCATION"));
					locationDetail.put("updateTimestamp", resultSet.getString("UPDATE_TIMESTAMP"));
					locationDetail.put("updateUser", resultSet.getString("UPDATE_USER"));
					locationDetail.put("noOfDays", resultSet.getString("NO_OF_DAYS"));
					locationDetail.put("userFullName", resultSet.getString("FULL_NAME"));
					locationHistoryList.add(locationDetail);
				}
			}
			negotiationVO.setNegotiationLocationHistoryList(locationHistoryList);

		} catch (Exception e) {
			logger.info("Exception in getNegotiationsLocationHistory : {}", e.getMessage());
		}
		return negotiationVO;
	}

	@Override
	public LastLocationDetails getLastLocationDetails(Integer negotiationId) {
		LastLocationDetails lastLocationDetails = new LastLocationDetails();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<NegotiationsLocation> rootNegotiationsLocation = query.from(NegotiationsLocation.class);
		query.where(builder.equal(rootNegotiationsLocation.get(NEGOTIATION_ID), negotiationId));
		query.select(builder.max(rootNegotiationsLocation.get(NEGOTIATION_LOCATION_ID)));
		if (session.createQuery(query).uniqueResult() != null) {
			int maxId = session.createQuery(query).uniqueResult();
			String typeCode = "";
			Timestamp oldtimeStamp = null;
			String lastLocationName = "";
			int daysCount;
			NegotiationsLocation negotiationsLocation = getNegotiationsLocationDetailsById(maxId);
			typeCode = negotiationsLocation.getLocationTypeCode();
			oldtimeStamp = negotiationsLocation.getUpdateTimestamp();
			lastLocationName = getLocationDescriptionByCode(typeCode);
			daysCount = calculateNumberOfDays(oldtimeStamp);
			lastLocationDetails.setLastLocation(lastLocationName);
			lastLocationDetails.setNumberOfDays(daysCount);
		} else {
			lastLocationDetails.setLastLocation("Not Available");
		}
		return lastLocationDetails;
	}

	private int calculateNumberOfDays(Timestamp oldtimeStamp) {
		try {
			Timestamp currentdate = commonDao.getCurrentTimestamp();
			long diff = currentdate.getTime() - oldtimeStamp.getTime();
			return (int) TimeUnit.DAYS.convert(diff, TimeUnit.MILLISECONDS);
		} catch (Exception e) {
			e.printStackTrace();
			return 0;
		}
	}

	private String getLocationDescriptionByCode(String typeCode) {
		return hibernateTemplate.get(NegotiationsLocationType.class, typeCode).getDescription();
	}

	private NegotiationsLocation getNegotiationsLocationDetailsById(int negotiationLocationId) {
		return hibernateTemplate.get(NegotiationsLocation.class, negotiationLocationId);
	}

	@Override
	public NegotiationsAgreementValue addAgreementValuePeriod(NegotiationsAgreementValue negotiationsAgreementValue) {
		hibernateTemplate.saveOrUpdate(negotiationsAgreementValue);
		return negotiationsAgreementValue;
	}

	public String deleteAgreementValuePeriod(NegotiationsAgreementValue negotiationsAgreementValue) {
		hibernateTemplate.delete(negotiationsAgreementValue);
		return commonDao.convertObjectToJSON("Negotiation Agreement Value Period deleted successfully");
	}

	@Override
	public NegotiationsPersonnel addPerson(NegotiationsPersonnel negotiationPersonnel) {
		hibernateTemplate.saveOrUpdate(negotiationPersonnel);
		return negotiationPersonnel;
	}

	@Override
	public String deletePersonnel(NegotiationsPersonnel negotiationsPersonnel) {
		hibernateTemplate.delete(negotiationsPersonnel);
		return commonDao.convertObjectToJSON("Negotiations Personnel deleted successfully");
	}

	@Override
	public String addNegotiationsAssociationDetails(NegotiationsAssociationDetails associationDetails) {
		hibernateTemplate.saveOrUpdate(associationDetails);
		return commonDao.convertObjectToJSON("Negotiation Association Details added successfully");
	}

	@Override
	public List<SponsorSearchResult> findSponsor() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("unchecked")
		Query<SponsorSearchResult> query = session
				.createQuery("SELECT NEW com.polus.fibicomp.vo.SponsorSearchResult(t.sponsorCode, t.sponsorName) "
						+ "FROM Sponsor t ");
		return query.list();
	}

	@Override
	public List<Organization> organizationSubAward() {
		return hibernateTemplate.loadAll(Organization.class);
	}
	
	@Override
	public HashMap<String, Object> getProjectDetailsSP(String moduleKey,String moduleNumber) {
 		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement statement = null;
		ResultSet resultSet = null;
		int code = Integer.valueOf(moduleKey);
		HashMap<String, Object> detailsField = new HashMap<String, Object>();
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
			String procedureName = "GET_MORE_PROJECT_DETAILS";
			String functionCall = "{call " + procedureName + "(?,?,?)}";
			statement = connection.prepareCall(functionCall);
			statement.setInt(1, code);
			statement.setString(2, moduleNumber);
			statement.registerOutParameter(3, OracleTypes.CURSOR);
			statement.execute();
			resultSet = (ResultSet) statement.getObject(3);
			} else if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				statement = connection.prepareCall("{call GET_MORE_PROJECT_DETAILS(?,?)}");
				statement.setInt(1, code);
				statement.setString(2, moduleNumber);
				statement.execute();
				resultSet = statement.getResultSet();
			}
			if (resultSet != null) {
				while (resultSet.next()) {
					if (code == 5) {
						detailsField.put("negotiationId", resultSet.getString("NEGOTIATION_ID"));
						detailsField.put("negotiationStatusCode", resultSet.getString("NEGOTIATION_STATUS_CODE"));
						detailsField.put("agreementTypeCode", resultSet.getString("AGREEMENT_TYPE_CODE"));
						detailsField.put("negotiatorPersonId", resultSet.getString("NEGOTIATOR_PERSON_ID"));
						detailsField.put("negotiatorFullName", resultSet.getString("NEGOTIATOR_FULL_NAME"));
						detailsField.put("startDate", resultSet.getString("START_DATE"));
						detailsField.put("endDate", resultSet.getString("END_DATE"));
						detailsField.put("updateTimeStamp", resultSet.getString("UPDATE_TIMESTAMP"));
						detailsField.put("updateUser", resultSet.getString("UPDATE_USER"));
						detailsField.put("createUser", resultSet.getString("CREATE_USER"));
						detailsField.put("createTimeStamp", resultSet.getString("CREATE_TIMESTAMP"));
						detailsField.put("summaryComment", resultSet.getString("SUMMARY_COMMENT"));
						detailsField.put("negotiatorComment", resultSet.getString("NEGOTIATOR_COMMENT"));
						detailsField.put("legalComment", resultSet.getString("LEGAL_COMMENT"));
						logger.info("negotiation details: {}" , detailsField);
					}
				}
			}
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		return detailsField;
	}
	
	@Override
	public List<NegotiationProjectDetailsDto> getDetailsFromProjectId(NegotiationVO vo) {
		List<NegotiationProjectDetailsDto> associationProjectList = new ArrayList<>();
		List<HashMap<String, Object>> associatedProjectList = new ArrayList<HashMap<String, Object>>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet resultSet = null;
		if((vo.getNegotiations().getAssociationsTypeCode() !=null) && (vo.getNegotiations().getAssociationsTypeCode().equalsIgnoreCase(Constants.ASSOCIATION_AWARDTYPE) || vo.getNegotiations().getAssociationsTypeCode().equalsIgnoreCase(Constants.ASSOCIATION_IPTYPE))) {
		try {
			if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
				String procedureName = "GET_MORE_PROJECT_DETAILS";
				String functionCall = "{call " + procedureName + "(?,?,?)}";
				callstm = connection.prepareCall(functionCall);
				callstm.setString(1, vo.getNegotiations().getAssociationsTypeCode());
				callstm.setString(2, vo.getNegotiations().getNegotiationsAssociations().get(0).getAssociatedProjectId());
				callstm.registerOutParameter(3, OracleTypes.CURSOR);
				callstm.execute();
				resultSet = (ResultSet) callstm.getObject(3);
			} else if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
				callstm = connection.prepareCall("{call GET_MORE_PROJECT_DETAILS(?,?)}");
				callstm.setString(1, vo.getNegotiations().getAssociationsTypeCode());
				callstm.setString(2, vo.getNegotiations().getNegotiationsAssociations().get(0).getAssociatedProjectId());
				callstm.execute();
				resultSet = callstm.getResultSet();	
			}
			while (resultSet.next()) {
				HashMap<String, Object> detailsField = new HashMap<String, Object>();
				if (vo.getNegotiations().getAssociationsTypeCode().equalsIgnoreCase(Constants.switchToAward)) {
					detailsField.put("AWARD_ID", resultSet.getString("AWARD_ID"));
					detailsField.put("AWARD_NUMBER", resultSet.getString("AWARD_NUMBER"));
					detailsField.put("AWARD_STATUS", resultSet.getString("AWARD_STATUS"));
					detailsField.put("PI_NAME", resultSet.getString("PI_NAME"));
					detailsField.put("LEAD_UNIT_NAME", resultSet.getString("LEAD_UNIT_NAME"));
					detailsField.put("SPONSOR_NAME", resultSet.getString("SPONSOR_NAME"));
					detailsField.put("LEAD_UNIT_NUMBER", resultSet.getString("LEAD_UNIT_NUMBER"));
					detailsField.put("TITLE", resultSet.getString("TITLE"));
					detailsField.put("ACCOUNT_NUMBER", resultSet.getString("ACCOUNT_NUMBER"));
				}
				if (vo.getNegotiations().getAssociationsTypeCode().equalsIgnoreCase(Constants.switchToIP)) {
					detailsField.put("SPONSOR", resultSet.getString("SPONSOR"));
					detailsField.put("TITLE", resultSet.getString("TITLE"));
					detailsField.put("UNIT_NAME", resultSet.getString("UNIT_NAME"));
					detailsField.put("HOME_UNIT_NUMBER", resultSet.getString("HOME_UNIT_NUMBER"));
					detailsField.put("PROPOSAL_NUMBER", resultSet.getString("PROPOSAL_NUMBER"));
					detailsField.put("PRIME_SPONSOR", resultSet.getString("PRIME_SPONSOR"));
					detailsField.put("PROPOSAL_ID", resultSet.getString("PROPOSAL_ID"));
					detailsField.put("PROPOSAL_TYPE", resultSet.getString("PROPOSAL_TYPE"));
					detailsField.put("ACTIVITY_TYPE", resultSet.getString("ACTIVITY_TYPE"));
					detailsField.put("INVESTIGATOR", resultSet.getString("INVESTIGATOR"));
					detailsField.put("STATUS", resultSet.getString("STATUS"));
				}
				associatedProjectList.add(detailsField);
			}
			if (!associatedProjectList.isEmpty()) {
				ArrayList<NegotiationProjectDetailsDto> parentHierarchyList = new ArrayList<>();
				for (int index = 0; index < associatedProjectList.size(); index++) {
					HashMap<String, Object> hmResult = associatedProjectList.get(index);
					NegotiationProjectDetailsDto projectDetailsDto = new NegotiationProjectDetailsDto();
					if(vo.getNegotiations().getAssociationsTypeCode().equalsIgnoreCase(Constants.switchToAward)) {
						projectDetailsDto.setAward_id((String) hmResult.get("AWARD_ID"));
						projectDetailsDto.setAward_number((String) hmResult.get("AWARD_NUMBER"));
						projectDetailsDto.setStatus((String) hmResult.get("AWARD_STATUS"));
						projectDetailsDto.setPi_name((String) hmResult.get("PI_NAME"));
						projectDetailsDto.setLead_unit_name((String) hmResult.get("LEAD_UNIT_NAME"));
						projectDetailsDto.setLead_unit_number((String) hmResult.get("LEAD_UNIT_NUMBER"));
						projectDetailsDto.setSponsor((String) hmResult.get("SPONSOR_NAME"));
						projectDetailsDto.setTitle((String) hmResult.get("TITLE"));
						projectDetailsDto.setAccount_number((String) hmResult.get("ACCOUNT_NUMBER"));
					}
					else if(vo.getNegotiations().getAssociationsTypeCode().equalsIgnoreCase(Constants.switchToIP)) {
						projectDetailsDto.setSponsor((String) hmResult.get("SPONSOR"));
						projectDetailsDto.setTitle((String) hmResult.get("TITLE"));
						projectDetailsDto.setLead_unit_name((String) hmResult.get("UNIT_NAME"));
						projectDetailsDto.setLead_unit_number((String) hmResult.get("HOME_UNIT_NUMBER"));
						projectDetailsDto.setProposalNumber((String) hmResult.get("PROPOSAL_NUMBER"));
						projectDetailsDto.setPrimeSponsor((String) hmResult.get("PRIME_SPONSOR"));
						projectDetailsDto.setType((String) hmResult.get("PROPOSAL_TYPE"));	
						projectDetailsDto.setProposal_id((String) hmResult.get("PROPOSAL_ID"));
						projectDetailsDto.setPi_name((String) hmResult.get("INVESTIGATOR"));
						projectDetailsDto.setStatus((String) hmResult.get("STATUS"));
						projectDetailsDto.setCategory((String) hmResult.get("ACTIVITY_TYPE"));
					}
					parentHierarchyList = new ArrayList<NegotiationProjectDetailsDto>();
					parentHierarchyList.add(projectDetailsDto);
				}
				associationProjectList = parentHierarchyList;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		}
		return associationProjectList;
	}

	@Override
	public NegotiationsAttachment deleteAttachment(Integer attachmentId) {
		NegotiationsAttachment negotiationsAttachmentList = hibernateTemplate.get(NegotiationsAttachment.class, attachmentId);
		FileData filedata = hibernateTemplate.get(FileData.class, negotiationsAttachmentList.getFileId());
		if (filedata != null) {
			hibernateTemplate.delete(negotiationsAttachmentList);
			hibernateTemplate.delete(filedata);
		}
		return negotiationsAttachmentList;
	}
		 	
	@Override
	public byte[] getTemplateData(NegotiationDataBus negotiationDataBus) {
		byte[] data = null;
		String templateTypeCode = null;
		try {
			if (negotiationDataBus.getModuleCode().equals(5)) {
				templateTypeCode = getLetterTemplateTypeCode(negotiationDataBus);
			}
			data = getLetterTemplate(templateTypeCode);
		} catch (Exception e) {
			logger.error("Exception in getTemplateData {}", e.getMessage());
		}
		return data;
	}
	
	private String getLetterTemplateTypeCode(NegotiationDataBus negotiationDataBus) {
		String templateTypeCode = null;
		try {
			String query = QueryBuilder.selectLetterTypeCodeForNegotiations(negotiationDataBus);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
			if (!dataList.isEmpty()) {
				templateTypeCode = (dataList.get(0).get("LETTER_TEMPLATE_TYPE_CODE").toString());
			}
		} catch (Exception e) {
			logger.error("Exception in getLetterTemplateTypeCode {}", e.getMessage());
		}
		return templateTypeCode;
	}
	
	private byte[] getLetterTemplate(String templateTypeCode) {
		byte[] data = null;
		try {
			String query = QueryBuilder.selectLetterTemplateforNegotiations(templateTypeCode);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
			if (!dataList.isEmpty()) {
				java.io.ByteArrayOutputStream baos = (java.io.ByteArrayOutputStream) dataList.get(0)
						.get("CORRESPONDENCE_TEMPLATE");
				data = baos.toByteArray();
			}
		} catch (Exception e) {
			logger.error("Exception in getLetterTemplate : {}", e.getMessage());
		}
		return data;
	}

	@Override
	public NegotiationReportDto fetchNegotiationsData(NegotiationDataBus negotiationDataBus) {
		NegotiationReportDto negotiationReportDto = new NegotiationReportDto();
		try {
			negotiationReportDto = getNegotiationFields(negotiationDataBus, negotiationReportDto);
		} catch (Exception e) {
			logger.error("Exception in fetchNegotiation Report : {}", e.getMessage());
		}
		return negotiationReportDto;
	}

	private NegotiationReportDto getNegotiationFields(NegotiationDataBus negotiationDataBus,
			NegotiationReportDto negotiationReportDto) throws Exception {
		String query = QueryBuilder.selectQueryForNegotiationDetails(negotiationDataBus);
		String locationQuery = QueryBuilder.selectQueryForNegotiationLocation(negotiationDataBus);
		ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
		ArrayList<HashMap<String, Object>> locationDataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(),
				locationQuery);
		if (dataList.get(0).get("FULL_NAME") != null)
			negotiationReportDto.setNegotiatorName(dataList.get(0).get("FULL_NAME").toString());
		if (dataList.get(0).get("NEGOTIATION_STATUS") != null)
			negotiationReportDto.setStatusCode(dataList.get(0).get("NEGOTIATION_STATUS").toString());
		if (dataList.get(0).get("START_DATE") != null)
			negotiationReportDto.setStart_date(dataList.get(0).get("START_DATE").toString());
		if (dataList.get(0).get("AGREEMENT_TYPE").toString() != null)
			negotiationReportDto.setAgreement_type(dataList.get(0).get("AGREEMENT_TYPE").toString());
		if (dataList.get(0).get("LEGAL_COMMENT") != null)
			negotiationReportDto.setLegal_comment(dataList.get(0).get("LEGAL_COMMENT").toString());
		if (dataList.get(0).get("SUMMARY_COMMENT") != null)
			negotiationReportDto.setSummary_comment(dataList.get(0).get("SUMMARY_COMMENT").toString());
		if (dataList.get(0).get("NEGOTIATOR_COMMENT") != null)
			negotiationReportDto.setNeotiator_comment(dataList.get(0).get("NEGOTIATOR_COMMENT").toString());
		if (dataList.get(0).get("CREATE_USER") != null)
			negotiationReportDto.setCreatedBy(dataList.get(0).get("CREATE_USER").toString());
		if (dataList.get(0).get("END_DATE") != null)
			negotiationReportDto.setEnd_date(dataList.get(0).get("END_DATE").toString());
		if ((!locationDataList.isEmpty()) && (locationDataList.get(0).get("LOCATION_TYPE").toString() != null))
			negotiationReportDto.setLocation(locationDataList.get(0).get("LOCATION_TYPE").toString());
		return negotiationReportDto;
	}

	private IContext setPlaceHolderData(IContext context, NegotiationReportDto negotiationReportDto) {
		context.put("NEGOTIATOR_NAME",
				negotiationReportDto.getNegotiatorName() == null ? "" : negotiationReportDto.getNegotiatorName());
		context.put("NEGOTIATIONS_STATUS",
				negotiationReportDto.getStatusCode() == null ? "" : negotiationReportDto.getStatusCode());
		context.put("START_DATE",
				negotiationReportDto.getStart_date() == null ? "" : negotiationReportDto.getStart_date());
		context.put("END_DATE", negotiationReportDto.getEnd_date() == null ? "" : negotiationReportDto.getEnd_date());
		context.put("AgreementType",
				negotiationReportDto.getAgreement_type() == null ? "" : negotiationReportDto.getAgreement_type());
		context.put("summaryComment",
				negotiationReportDto.getSummary_comment() == null ? "" : negotiationReportDto.getSummary_comment());
		context.put("legalComment",
				negotiationReportDto.getLegal_comment() == null ? "" : negotiationReportDto.getLegal_comment());
		context.put("NegotiatorComment",
				negotiationReportDto.getNeotiator_comment() == null ? "" : negotiationReportDto.getNeotiator_comment());
		context.put("Location", negotiationReportDto.getLocation() == null ? "" : negotiationReportDto.getLocation());
		context.put("CreatedBy",
				negotiationReportDto.getCreatedBy() == null ? "" : negotiationReportDto.getCreatedBy());
		return context;
	}
	
	@Override
	public byte[] mergePlaceHolders(String outputDataFormat, byte[] data, NegotiationReportDto negotiationReportDto) {
		byte[] mergedOutput = null;
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream,
					TemplateEngineKind.Velocity);
			IContext context = report.createContext();
			context = setPlaceHolderData(context, negotiationReportDto);
			DocxDocumentMergerAndConverter docxDocumentMergerAndConverter = new DocxDocumentMergerAndConverter();
			if (outputDataFormat.equalsIgnoreCase("pdf")) {
//				mergedOutput = docxDocumentMergerAndConverter.mergeAndGeneratePDFOutput(myInputStream, report, null,
//						TemplateEngineKind.Velocity, context);
			} else {
//				mergedOutput = docxDocumentMergerAndConverter.generateMergedOutput(report, context);
			}
		} catch (Exception e) {
			logger.error("Exception in mergePlaceHolders : {}", e.getMessage());
		}
		return mergedOutput;
	}

	@Override
	public NegotiationProjectDetailsDto getDetailsFromProjectId(NegotiationsAssociation VO) {
		NegotiationProjectDetailsDto associationProjectList = new NegotiationProjectDetailsDto();
		List<HashMap<String, Object>> associatedProjectList = new ArrayList<HashMap<String, Object>>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		SessionImpl sessionImpl = (SessionImpl) session;
		Connection connection = sessionImpl.connection();
		CallableStatement callstm = null;
		ResultSet resultSet = null;
		if ((VO.getAssociationTypeCode() != null) && (VO.getAssociationTypeCode().equalsIgnoreCase(Constants.ASSOCIATION_AWARDTYPE) || VO.getAssociationTypeCode().equalsIgnoreCase(Constants.ASSOCIATION_IPTYPE))) {
			try {
				if (oracledb.equalsIgnoreCase(Constants.dbOracle)) {
					String procedureName = "GET_MORE_PROJECT_DETAILS";
					String functionCall = "{call " + procedureName + "(?,?,?)}";
					callstm = connection.prepareCall(functionCall);
					callstm.setInt(1, Integer.parseInt(VO.getAssociationTypeCode()));
					callstm.setString(2, VO.getAssociatedProjectId());
					callstm.registerOutParameter(3, OracleTypes.CURSOR);
					callstm.execute();
					resultSet = (ResultSet) callstm.getObject(3);
				} else if (oracledb.equalsIgnoreCase(Constants.dbMySQL)) {
					callstm = connection.prepareCall("{call GET_MORE_PROJECT_DETAILS(?,?)}");
					callstm.setInt(1, Integer.parseInt(VO.getAssociationTypeCode()));
					callstm.setString(2, VO.getAssociatedProjectId());
					callstm.execute();
					resultSet = callstm.getResultSet();
				}
				while (resultSet != null && resultSet.next()) {
					HashMap<String, Object> detailsField = new HashMap<String, Object>();
					if (VO.getAssociationTypeCode().equalsIgnoreCase(Constants.switchToAward)) {
						detailsField.put("AWARD_ID", resultSet.getString("AWARD_ID"));
						detailsField.put("AWARD_NUMBER", resultSet.getString("AWARD_NUMBER"));
						detailsField.put("AWARD_STATUS", resultSet.getString("AWARD_STATUS"));
						detailsField.put("PI_NAME", resultSet.getString("PI_NAME"));
						detailsField.put("LEAD_UNIT_NAME", resultSet.getString("LEAD_UNIT_NAME"));
						detailsField.put("SPONSOR_NAME", resultSet.getString("SPONSOR_NAME"));
						detailsField.put("LEAD_UNIT_NUMBER", resultSet.getString("LEAD_UNIT_NUMBER"));
						detailsField.put("TITLE", resultSet.getString("TITLE"));
						detailsField.put("ACCOUNT_NUMBER", resultSet.getString("ACCOUNT_NUMBER"));
					}
					if (VO.getAssociationTypeCode().equalsIgnoreCase(Constants.switchToIP)) {
						detailsField.put("SPONSOR", resultSet.getString("SPONSOR"));
						detailsField.put("TITLE", resultSet.getString("TITLE"));
						detailsField.put("UNIT_NAME", resultSet.getString("UNIT_NAME"));
						detailsField.put("PROPOSAL_NUMBER", resultSet.getString("PROPOSAL_NUMBER"));
						detailsField.put("PRIME_SPONSOR", resultSet.getString("PRIME_SPONSOR"));
						detailsField.put("PROPOSAL_ID", resultSet.getString("PROPOSAL_ID"));
						detailsField.put("PROPOSAL_TYPE", resultSet.getString("PROPOSAL_TYPE"));
						detailsField.put("INVESTIGATOR", resultSet.getString("INVESTIGATOR"));
						detailsField.put("STATUS", resultSet.getString("STATUS"));
						detailsField.put("ACTIVITY_TYPE", resultSet.getString("ACTIVITY_TYPE"));
					}
					associatedProjectList.add(detailsField);
				}
				if (!associatedProjectList.isEmpty()) {
					for (int index = 0; index < associatedProjectList.size(); index++) {
						HashMap<String, Object> hmResult = associatedProjectList.get(index);
						NegotiationProjectDetailsDto projectDetailsDto = new NegotiationProjectDetailsDto();
						if (VO.getAssociationTypeCode().equalsIgnoreCase(Constants.switchToAward)) {
							projectDetailsDto.setAward_id((String) hmResult.get("AWARD_ID"));
							projectDetailsDto.setAward_number((String) hmResult.get("AWARD_NUMBER"));
							projectDetailsDto.setStatus((String) hmResult.get("AWARD_STATUS"));
							projectDetailsDto.setPi_name((String) hmResult.get("PI_NAME"));
							projectDetailsDto.setLead_unit_name((String) hmResult.get("LEAD_UNIT_NAME"));
							projectDetailsDto.setLead_unit_number((String) hmResult.get("LEAD_UNIT_NUMBER"));
							projectDetailsDto.setSponsor((String) hmResult.get("SPONSOR_NAME"));
							projectDetailsDto.setTitle((String) hmResult.get("TITLE"));
							projectDetailsDto.setAccount_number((String) hmResult.get("ACCOUNT_NUMBER"));
						} else if (VO.getAssociationTypeCode().equalsIgnoreCase(Constants.switchToIP)) {
							projectDetailsDto.setSponsor((String) hmResult.get("SPONSOR"));
							projectDetailsDto.setTitle((String) hmResult.get("TITLE"));
							projectDetailsDto.setLead_unit_name((String) hmResult.get("UNIT_NAME"));
							projectDetailsDto.setLead_unit_number((String) hmResult.get("HOME_UNIT_NUMBER"));
							projectDetailsDto.setProposalNumber((String) hmResult.get("PROPOSAL_NUMBER"));
							projectDetailsDto.setPrimeSponsor((String) hmResult.get("PRIME_SPONSOR"));
							projectDetailsDto.setType((String) hmResult.get("PROPOSAL_TYPE"));
							projectDetailsDto.setProposal_id((String) hmResult.get("PROPOSAL_ID"));
							projectDetailsDto.setPi_name((String) hmResult.get("INVESTIGATOR"));
							projectDetailsDto.setStatus((String) hmResult.get("STATUS"));
							projectDetailsDto.setCategory((String) hmResult.get("ACTIVITY_TYPE"));
						}
						associationProjectList = projectDetailsDto;
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return associationProjectList;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<NegotiationsAssociation> fetchAssociationData(Integer negotiationId) {
		List<NegotiationsAssociation> negotiationsAssociation = new ArrayList<>();
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		@SuppressWarnings("deprecation")
		Criteria criteria = session.createCriteria(NegotiationsAssociation.class);
		criteria.add(Restrictions.like("negotiationId", negotiationId));
		negotiationsAssociation = criteria.list();
		return negotiationsAssociation;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public String deleteNegotiationAssociationDetailsByAssociationId(Integer negotiationsAssociationId) {
		Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(
				"delete from NegotiationsAssociationDetails d where d.negotiationsAssociationId = :associationId");
		query.setParameter("associationId", negotiationsAssociationId);
		int rowCount = query.executeUpdate();
		logger.info("Rows affected: {}", rowCount);
		return commonDao.convertObjectToJSON("success");
	}

	@Override
	public List<OrganizationSearchResult> findSubawardOrganisations(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		final String likeCriteria = "%" + searchString.toUpperCase() + "%";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<OrganizationSearchResult> query = session.createQuery("SELECT NEW com.polus.fibicomp.vo.OrganizationSearchResult(t.organizationId, t.organizationName) " +
                "FROM Organization t " +
                "WHERE UPPER(t.organizationId) like :likeCriteria or UPPER(t.organizationName) like :likeCriteria");
		query.setParameter("likeCriteria", likeCriteria);
		return ListUtils.emptyIfNull(query.setMaxResults(25).list());
	}

	private void loadNegotiationUserFullNames(Negotiations negotiation) {
		if (negotiation.getCreateUser() != null) {
			negotiation.setCreateUserFullName(personDao.getUserFullNameByUserName(negotiation.getCreateUser()));
		}
		if (negotiation.getUpdateUser() != null) {
			negotiation.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiation.getUpdateUser()));
		}
		if (negotiation.getNegotiationsActivities() != null && !negotiation.getNegotiationsActivities().isEmpty()) {
			for (NegotiationsActivity negotiationsActivity : negotiation.getNegotiationsActivities()) {
				if (negotiationsActivity.getUpdateUser() != null) {
					negotiationsActivity.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsActivity.getUpdateUser()));
				}				
			}
		}
		if (negotiation.getNegotiationsAttachments() != null && !negotiation.getNegotiationsAttachments().isEmpty()) {
			for (NegotiationsAttachment negotiationsAttachment : negotiation.getNegotiationsAttachments()) {
				if (negotiationsAttachment.getUpdateUser() != null) {
					negotiationsAttachment.setUpdateUserFullName(personDao.getUserFullNameByUserName(negotiationsAttachment.getUpdateUser()));
				}
			}
		}
		if (negotiation.getNegotiationsAssociationDetails() != null && !negotiation.getNegotiationsAssociationDetails().isEmpty()) {
			for (NegotiationsAssociationDetails associationDetails : negotiation.getNegotiationsAssociationDetails()) {
				if (associationDetails.getUpdateUser() != null) {
					associationDetails.setUpdateUserFullName(personDao.getUserFullNameByUserName(associationDetails.getUpdateUser()));
				}				
			}
		}
	}

	@Override
	public String getSubAwardOrganizationName(Integer negotiationId) {
		String organizationName = "";
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsAssociationDetails> subAwardOrganizationQuery = builder.createQuery(NegotiationsAssociationDetails.class);
		Root<NegotiationsAssociationDetails> rootgetSubAwardOrganizationName = subAwardOrganizationQuery.from(NegotiationsAssociationDetails.class);
		subAwardOrganizationQuery.where(builder.equal(rootgetSubAwardOrganizationName.get("negotiationId"), negotiationId.toString())); 
		List<NegotiationsAssociationDetails> negotiationAssociations = session.createQuery(subAwardOrganizationQuery).getResultList();
		if (negotiationAssociations != null && !negotiationAssociations.isEmpty()) {
			NegotiationsAssociationDetails negotiationsAssociationDetails = negotiationAssociations.get(0);
			String subAwardId = negotiationsAssociationDetails.getSubAwardOrg();
			if (subAwardId != null) {
				organizationName = getOrgnizationById(subAwardId);
			}
		}
		return organizationName;
	}
}
