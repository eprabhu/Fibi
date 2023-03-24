package com.polus.fibicomp.agreements.dao;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import javax.persistence.ParameterMode;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.hibernate.procedure.ProcedureCall;
import org.hibernate.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.agreements.pojo.AgreementActionLog;
import com.polus.fibicomp.agreements.pojo.AgreementActionType;
import com.polus.fibicomp.agreements.pojo.AgreementAssociationDetail;
import com.polus.fibicomp.agreements.pojo.AgreementAssociationLink;
import com.polus.fibicomp.agreements.pojo.AgreementAttachment;
import com.polus.fibicomp.agreements.pojo.AgreementAttachmentFile;
import com.polus.fibicomp.agreements.pojo.AgreementAttachmentStatus;
import com.polus.fibicomp.agreements.pojo.AgreementAttachmentType;
import com.polus.fibicomp.agreements.pojo.AgreementCategory;
import com.polus.fibicomp.agreements.pojo.AgreementClauses;
import com.polus.fibicomp.agreements.pojo.AgreementClausesGroupMapping;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementNote;
import com.polus.fibicomp.agreements.pojo.AgreementNoteAttachment;
import com.polus.fibicomp.agreements.pojo.AgreementPeople;
import com.polus.fibicomp.agreements.pojo.AgreementPeopleType;
import com.polus.fibicomp.agreements.pojo.AgreementPlaceHolder;
import com.polus.fibicomp.agreements.pojo.AgreementReviewType;
import com.polus.fibicomp.agreements.pojo.AgreementSponsor;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorContact;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorContactType;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorType;
import com.polus.fibicomp.agreements.pojo.AgreementStatus;
import com.polus.fibicomp.agreements.pojo.AgreementType;
import com.polus.fibicomp.agreements.pojo.AgreementTypeHistory;
import com.polus.fibicomp.agreements.pojo.AgreementTypeTemplate;
import com.polus.fibicomp.agreements.pojo.AgreementWorkflowStatus;
import com.polus.fibicomp.agreements.pojo.Clauses;
import com.polus.fibicomp.agreements.pojo.ClausesBank;
import com.polus.fibicomp.agreements.pojo.ClausesGroup;
import com.polus.fibicomp.agreements.pojo.SponsorRole;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.DBEngineConstants;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.fieldwiseedit.pojo.FieldSectionMapping;
import com.polus.fibicomp.fieldwiseedit.pojo.RightSectionMapping;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociation;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.utils.DashBoardQueries;

@Transactional
@Service(value = "agreementDao")
public class AgreementDaoImpl implements AgreementDao {

	protected static Logger logger = LogManager.getLogger(AgreementDaoImpl.class.getName());

	private static final String AGREEMENT_REQUEST_ID = "agreementRequestId";
	private static final String UPDATE_TIMESTAMP = "updateTimestamp";
	private static final String AGREEMENT_REQUEST_ID_PARAMETER = "<<AGREEMENT_REQUEST_ID>>";

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	public DBEngine dbEngine;
	
	@Autowired
	private CommonDao commonDao;

	@Override
	public AgreementHeader saveOrUpdateAgreement(AgreementHeader agreement) {
		hibernateTemplate.saveOrUpdate(agreement);
		return agreement;
	}

	@Override
	public List<AgreementType> fetchAgreementTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementType> query = builder.createQuery(AgreementType.class);
		Root<AgreementType> rootAgreementCategory = query.from(AgreementType.class);
		query.orderBy(builder.asc(rootAgreementCategory.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AgreementHeader getAgreementById(Integer agreementRequestId) {
		return hibernateTemplate.get(AgreementHeader.class, agreementRequestId);
	}

	@Override
	public AgreementNote saveOrUpdateAgreementNotes(AgreementNote agreementNote) {
		hibernateTemplate.saveOrUpdate(agreementNote);
		return agreementNote;
	}

	@Override
	public String deleteAgreementNote(AgreementNote agreementNote) {
		hibernateTemplate.delete(agreementNote);
		return "Agreement Note deleted successfully";
	}

	@Override
	public List<AgreementActionType> fetchAgreementActionTypes() {
		return hibernateTemplate.loadAll(AgreementActionType.class);
	}

	@Override
	public String deleteAgreement(AgreementHeader agreement) {
		hibernateTemplate.delete(agreement);
		return "Agreement deleted successfully";
	}

	@Override
	public List<AgreementStatus> fetchAgreementStatusTypes() {
		return hibernateTemplate.loadAll(AgreementStatus.class);
	}

	@Override
	public List<AgreementSponsorContactType> fetchAgreementSponsorContactTypes() {
		return hibernateTemplate.loadAll(AgreementSponsorContactType.class);
	}

	@Override
	public List<AgreementAttachmentType> fetchAgreementAttachmentTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementAttachmentType> query = builder.createQuery(AgreementAttachmentType.class);
		Root<AgreementAttachmentType> rootAgreementSponsorType = query.from(AgreementAttachmentType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AgreementSponsor saveOrUpdateAgreementSponsor(AgreementSponsor agreementSponsor) {
		hibernateTemplate.saveOrUpdate(agreementSponsor);
		return agreementSponsor;
	}

	@Override
	public String deleteAgreementSponsorContact(AgreementSponsorContact sponsorContact) {
		hibernateTemplate.delete(sponsorContact);
		return "Other party contact deleted successfully";
	}

	@Override
	public AgreementSponsorContact saveOrUpdateAgreementSponsorContact(AgreementSponsorContact sponsorContact) {
		hibernateTemplate.saveOrUpdate(sponsorContact);
		return sponsorContact;
	}

	@Override
	public String deleteAgreementSponsor(AgreementSponsor agreementSponsor) {
		hibernateTemplate.delete(agreementSponsor);
		return "Other party deleted successfully";
	}

	@Override
	public AgreementActionLog createAgreementActionLog(AgreementActionLog agreementActionLog) {
		hibernateTemplate.save(agreementActionLog);
		return agreementActionLog;
	}

	@Override
	public AgreementAttachmentFile saveFileData(AgreementAttachmentFile fileData) {
		hibernateTemplate.save(fileData);
		return fileData;
	}

	@Override
	public AgreementAttachmentFile getFileDataById(String agreementAttachmentFileId) {
		return hibernateTemplate.get(AgreementAttachmentFile.class, agreementAttachmentFileId);
	}

	@Override
	public void deleteFileData(AgreementAttachmentFile fileData) {
		hibernateTemplate.delete(fileData);
	}

	@Override
	public AgreementAttachment saveOrUpdateAgreementAttachment(AgreementAttachment attachment) {
		hibernateTemplate.saveOrUpdate(attachment);
		return attachment;
	}

	@Override
	public String deleteAgreementAttachments(AgreementAttachment agreementAttachment) {
		hibernateTemplate.delete(agreementAttachment);
		return "Agreement Attachment deleted successfully";
	}

	@Override
	public AgreementAttachment fetchAttachmentById(Integer agreementAttachmentId) {
		return hibernateTemplate.get(AgreementAttachment.class, agreementAttachmentId);
	}

	@Override
	public void deleteFileById(String agreementAttachmentFileId) {
		hibernateTemplate.delete(hibernateTemplate.get(AgreementAttachmentFile.class, agreementAttachmentFileId));
	}

	@Override
	public String deleteAgreementFileData(String agreementAttachmentFileId) {
		hibernateTemplate.delete(hibernateTemplate.get(AgreementAttachmentFile.class, agreementAttachmentFileId));
		return "Agreement Attachment deleted successfully";
	}

	@Override
	public List<AgreementAttachment> fetchAgreementAttachmentsBasedOnAgreementId(Integer agreementId, Boolean isGenerated) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementAttachment> query = builder.createQuery(AgreementAttachment.class);
		Root<AgreementAttachment> rootAgreementAttachment = query.from(AgreementAttachment.class);
		Predicate predicateOne = builder.equal(rootAgreementAttachment.get(AGREEMENT_REQUEST_ID), agreementId);
		Predicate predicateTwo;
		if (Boolean.FALSE.equals(isGenerated)) {
			predicateTwo = builder.notEqual(rootAgreementAttachment.get("agreementAttachmentTypeCode"), "4");
			query.where(builder.and(predicateOne, predicateTwo));
			query.orderBy(builder.desc(rootAgreementAttachment.get(UPDATE_TIMESTAMP)));
		} else {
			predicateTwo = builder.equal(rootAgreementAttachment.get("agreementAttachmentTypeCode"), "4");
			query.where(builder.and(predicateOne, predicateTwo));
			query.orderBy(builder.desc(rootAgreementAttachment.get("versionNumber")));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public AgreementActionType fetchAgreementActionTypeById(String agreementActionTypeId) {
		return hibernateTemplate.get(AgreementActionType.class, agreementActionTypeId);
	}

	@Override
	public AgreementStatus fetchAgreementStatusByStatusCode(String statusCode) {
		return hibernateTemplate.get(AgreementStatus.class, statusCode);
	}

	@Override
	public AgreementCategory fetchAgreementCategoryByCategoryCode(String agreementCategoryCode) {
		return hibernateTemplate.get(AgreementCategory.class, agreementCategoryCode);
	}

	@Override
	public List<AgreementCategory> fetchAllAgreementCategory() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementCategory> query = builder.createQuery(AgreementCategory.class);
		Root<AgreementCategory> rootAgreementCategory = query.from(AgreementCategory.class);
		query.orderBy(builder.asc(rootAgreementCategory.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AgreementAttachment> fetchAgreementAttachmentsBasedOnAgreementIdAndDocumentId(Integer agreementId, Integer documentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementAttachment> query = builder.createQuery(AgreementAttachment.class);
		Root<AgreementAttachment> rootAgreementAttachment = query.from(AgreementAttachment.class);
		Predicate predicateOne = builder.equal(rootAgreementAttachment.get(AGREEMENT_REQUEST_ID), agreementId);
		Predicate predicateTwo = builder.equal(rootAgreementAttachment.get("documentId"), documentId);
		query.where(builder.and(predicateOne, predicateTwo));
		query.orderBy(builder.desc(rootAgreementAttachment.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<NegotiationsAssociation> getNegotiationIdBasedOnAgreementId(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsAssociation> query = builder.createQuery(NegotiationsAssociation.class);
		Root<NegotiationsAssociation> rootNegotiationsAssociation = query.from(NegotiationsAssociation.class);
		Predicate predicate1 = builder.equal(rootNegotiationsAssociation.get("associatedProjectId"), agreementRequestId);
		Predicate predicate2 = builder.equal(rootNegotiationsAssociation.get("associationTypeCode"), "4");
		query.where(builder.and(predicate1, predicate2));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AgreementSponsor> getAgreementSponsors(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementSponsor> query = builder.createQuery(AgreementSponsor.class);
		Root<AgreementSponsor> rootAgreementSponsor = query.from(AgreementSponsor.class);
		query.where(builder.equal(rootAgreementSponsor.get(AGREEMENT_REQUEST_ID), agreementRequestId));
		query.orderBy(builder.desc(rootAgreementSponsor.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AgreementSponsorContact> getAgreementSponsorContacts(Integer agreementSponsorId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementSponsorContact> query = builder.createQuery(AgreementSponsorContact.class);
		Root<AgreementSponsorContact> rootAgreementSponsorContact = query.from(AgreementSponsorContact.class);
		query.where(builder.equal(rootAgreementSponsorContact.get("agreementSponsorId"), agreementSponsorId));
		query.orderBy(builder.desc(rootAgreementSponsorContact.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AgreementSponsor fetchAgreementSponsorBasedOnId(Integer agreementSponsorId) {
		return hibernateTemplate.get(AgreementSponsor.class, agreementSponsorId);
	}

	@Override
	public List<AgreementTypeTemplate> fetchAgreementTypeTemplate() {
		return hibernateTemplate.loadAll(AgreementTypeTemplate.class);
	}

	@Override
	public AgreementTypeTemplate saveOrUpdateAgreementTypeTemplate(AgreementTypeTemplate agreementTypeTemplate) {
		hibernateTemplate.saveOrUpdate(agreementTypeTemplate);
		return agreementTypeTemplate;
	}

	@Override
	public List<AgreementTypeTemplate> fetchAgreementAttachmentsBasedOnAgreementType(String agreementTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementTypeTemplate> query = builder.createQuery(AgreementTypeTemplate.class);
		Root<AgreementTypeTemplate> rootAgreementTypeTemplate = query.from(AgreementTypeTemplate.class);
		query.where(builder.equal(rootAgreementTypeTemplate.get("agreementTypeCode"), agreementTypeCode));
		query.orderBy(builder.desc(rootAgreementTypeTemplate.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ClausesGroup saveOrUpdateClausesGroup(ClausesGroup clausesGroup) {
		hibernateTemplate.saveOrUpdate(clausesGroup);
		return clausesGroup;
	}

	@Override
	public AgreementClausesGroupMapping saveOrUpdateAgreementClausesGroup(AgreementClausesGroupMapping agreementClausesGroupMapping) {
		hibernateTemplate.saveOrUpdate(agreementClausesGroupMapping);
		return agreementClausesGroupMapping;
	}

	@Override
	public List<AgreementPlaceHolder> getAllAgreementPlaceHolders() {
		return hibernateTemplate.loadAll(AgreementPlaceHolder.class);
	}

	@Override
	public List<ClausesGroup> getClausesDetails() {
		return hibernateTemplate.loadAll(ClausesGroup.class);
	}

	@Override
	public List<AgreementClausesGroupMapping> fetchAllAgreementTypeBasedOnClausesGroup(Integer clauseGroupCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementClausesGroupMapping> query = builder.createQuery(AgreementClausesGroupMapping.class);
		Root<AgreementClausesGroupMapping> rootAgreementClausesGroupMapping = query.from(AgreementClausesGroupMapping.class);
		query.where(builder.equal(rootAgreementClausesGroupMapping.get("clausesGroupCode"), clauseGroupCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Integer getMaxVersionNumber(String agreementTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<AgreementTypeTemplate> rootAward = query.from(AgreementTypeTemplate.class);
		Predicate predicateOne = builder.equal(rootAward.get("agreementTypeCode"), agreementTypeCode);
		query.select(builder.max(rootAward.get("versionNumber")));
		query.where(builder.and(predicateOne));
		if(session.createQuery(query).getSingleResult() != null) {
			return  session.createQuery(query).getSingleResult()+1;
		} else {
			return 1;
		}
	}

	@Override
	public List<AgreementClausesGroupMapping> getClausesDetailsByAgreementTypeCode(String agreementTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementClausesGroupMapping> query = builder.createQuery(AgreementClausesGroupMapping.class);
		Root<AgreementClausesGroupMapping> rootAgreementClausesGroupMapping = query.from(AgreementClausesGroupMapping.class);
		query.where(builder.equal(rootAgreementClausesGroupMapping.get("agreementTypeCode"), agreementTypeCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public Clauses fetchClausesById(Integer clausesCode) {
		return hibernateTemplate.get(Clauses.class, clausesCode);
	}

	@Override
	public void deleteClauses(Clauses clauses) {
		hibernateTemplate.delete(clauses);
	}

	@Override
	public AgreementClausesGroupMapping fetchAgreementGroupMapping(String agreementTypeCode, String clausesGroupCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementClausesGroupMapping> query = builder.createQuery(AgreementClausesGroupMapping.class);
		Root<AgreementClausesGroupMapping> rootAgreementClausesGroupMapping = query.from(AgreementClausesGroupMapping.class);
		Predicate predicateOne = builder.equal(rootAgreementClausesGroupMapping.get("agreementTypeCode"), agreementTypeCode);
		Predicate predicateTwo = builder.equal(rootAgreementClausesGroupMapping.get("clausesGroupCode"), clausesGroupCode);
		query.where(builder.and(predicateOne, predicateTwo));
		if (session.createQuery(query).getResultList() != null && !session.createQuery(query).getResultList().isEmpty()) {
			return session.createQuery(query).getResultList().get(0);
		}
		return null;
	}

	@Override
	public void deleteAgreementGroupMapping(AgreementClausesGroupMapping agreementClausesGroup) {
		hibernateTemplate.delete(agreementClausesGroup);		
	}

	@Override
	public AgreementTypeTemplate fetchAgreementTypeTemplateById(Integer templateId) {
		return hibernateTemplate.get(AgreementTypeTemplate.class, templateId);
	}

	@Override
	public void deleteAgreementTypeTemplate(AgreementTypeTemplate agreementTypeTemplate) {
		hibernateTemplate.delete(agreementTypeTemplate);	
	}

	@Override
	public List<AgreementActionLog> fetchAgreementActionLogsBasedOnAgreementId(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementActionLog> query = builder.createQuery(AgreementActionLog.class);
		Root<AgreementActionLog> rootAgreementActionLog = query.from(AgreementActionLog.class);
		query.where(builder.equal(rootAgreementActionLog.get(AGREEMENT_REQUEST_ID), agreementRequestId));
		query.orderBy(builder.desc(rootAgreementActionLog.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public byte[] fetchAgreementTemplateBasedOnParams(String agreementTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementTypeTemplate> query = builder.createQuery(AgreementTypeTemplate.class);
		Root<AgreementTypeTemplate> rootAward = query.from(AgreementTypeTemplate.class);
		Predicate predicateOne = builder.equal(rootAward.get("agreementTypeCode"), agreementTypeCode);
		builder.max(rootAward.get("versionNumber"));
		query.where(builder.and(predicateOne));
		if (session.createQuery(query).getResultList() != null && !session.createQuery(query).getResultList().isEmpty()) {
			AgreementTypeTemplate agreementTypeTemplate = session.createQuery(query).getResultList().get(0);
			return agreementTypeTemplate.getTemplate();
		}
		return new byte[0];
	}

	@Override
	public HashMap<String, Object> fetchAgreementViewBasedOnAgreementId(Integer agreementRequestId) {
		HashMap<String, Object> agreementViewDetail = null;
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(AGREEMENT_REQUEST_ID_PARAMETER, DBEngineConstants.TYPE_INTEGER, agreementRequestId));
		try {
			List<HashMap<String, Object>> agrementViewDetails = dbEngine.executeQuery(inParam, "GET_AGREEMENT_VIEW");
			if (agrementViewDetails != null && !agrementViewDetails.isEmpty()) {
				agreementViewDetail = agrementViewDetails.get(0);
			}
		} catch (Exception e) {
			logger.error("Exception in fetchAgreementViewBasedOnAgreementId : {}", e.getMessage());
		}
		return agreementViewDetail;
	}

	@Override
	public List<AgreementClauses> fetchAgreementClausesBasedOnAgreementId(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementClauses> query = builder.createQuery(AgreementClauses.class);
		Root<AgreementClauses> rootAgreementClause = query.from(AgreementClauses.class);
		query.where(builder.equal(rootAgreementClause.get(AGREEMENT_REQUEST_ID), agreementRequestId));
		query.orderBy(builder.asc(rootAgreementClause.get("clausesGroupCode")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<HashMap<String, Object>> fetchAgreementQuestionnaireBasedOnAgreementId(Integer agreementRequestId) {
		List<HashMap<String, Object>> agrementQuestionnaireDetails = null;
		ArrayList<Parameter> inParam = new ArrayList<>();
		inParam.add(new Parameter(AGREEMENT_REQUEST_ID_PARAMETER, DBEngineConstants.TYPE_STRING, agreementRequestId.toString()));
		try {
			agrementQuestionnaireDetails = dbEngine.executeQuery(inParam, "GET_AGREEMENT_QUESTIONNAIRE");
		} catch (Exception e) {
			logger.error("Exception in fetchAgreementQuestionnaireBasedOnAgreementId : {}", e.getMessage());
		}
		return agrementQuestionnaireDetails;
	}

	@Override
	public AgreementAttachmentType fetchAgreementAttachmentTypeBasedOnAttachmentTypeCode(String agreementAttachmentTypeCode) {
		return hibernateTemplate.load(AgreementAttachmentType.class, agreementAttachmentTypeCode);
	}

	@Override
	public Integer fetchMaxVersionAgreementAttachmentBasedOnParams(Integer agreementRequestId, String agreementAttachmentTypeCode) {		
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<AgreementAttachment> rootAgreementAttachment = query.from(AgreementAttachment.class);
		Predicate predicateOne = builder.equal(rootAgreementAttachment.get("agreementRequestId"), agreementRequestId);
		Predicate predicateTwo = builder.equal(rootAgreementAttachment.get("agreementAttachmentTypeCode"), agreementAttachmentTypeCode);
		query.select(builder.max(rootAgreementAttachment.get("versionNumber")));
		query.where(builder.and(predicateOne, predicateTwo));
		if (session.createQuery(query).getSingleResult() != null) {
			return session.createQuery(query).getSingleResult();
		}
		return 0;
	}

	@Override
	public List<AgreementAttachment> fetchAllAgreementAttachmentsBasedOnParams(Integer agreementRequestId, String agreementAttachmentTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementAttachment> query = builder.createQuery(AgreementAttachment.class);
		Root<AgreementAttachment> rootAgreementAttachment = query.from(AgreementAttachment.class);
		Predicate predicateOne = builder.equal(rootAgreementAttachment.get(AGREEMENT_REQUEST_ID), agreementRequestId);
		Predicate predicateTwo = builder.equal(rootAgreementAttachment.get("agreementAttachmentTypeCode"), agreementAttachmentTypeCode);
		query.where(builder.and(predicateOne, predicateTwo));
		query.orderBy(builder.desc(rootAgreementAttachment.get("versionNumber")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AgreementTypeTemplate fetchAgreementTypeTemplateBasedOnParams(String agreementTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementTypeTemplate> query = builder.createQuery(AgreementTypeTemplate.class);
		Root<AgreementTypeTemplate> rootAgreementTypeTemplate = query.from(AgreementTypeTemplate.class);
		Predicate predicateOne = builder.equal(rootAgreementTypeTemplate.get("agreementTypeCode"), agreementTypeCode);
		Predicate predicateTwo = builder.equal(rootAgreementTypeTemplate.get("documentId"), 1);
		Predicate predicateThree = builder.equal(rootAgreementTypeTemplate.get("documentStatusCode"), 1);
		query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		if (session.createQuery(query).getResultList() != null && !session.createQuery(query).getResultList().isEmpty()) {
			return session.createQuery(query).getResultList().get(0);
		}
		return null;
	}

	@Override
	public List<ClausesBank> findClauses(String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ClausesBank> query = builder.createQuery(ClausesBank.class);
		Root<ClausesBank> rootClauses = query.from(ClausesBank.class);
		Predicate predicate1 = builder.like(rootClauses.get("description"), "%" + searchString + "%");
		query.where(builder.and(predicate1));
		query.orderBy(builder.asc(rootClauses.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AgreementClauses saveOrUpdateAgreementClauses(AgreementClauses agreementClauses) {
		hibernateTemplate.saveOrUpdate(agreementClauses);
		return agreementClauses;
	}

	@Override
	public AgreementClauses fetchAgreementClausesBasedOnId(Integer agreementClauseId) {
		return hibernateTemplate.get(AgreementClauses.class, agreementClauseId);
	}

	@Override
	public void deleteAgreementClauses(AgreementClauses agreementClause) {
		hibernateTemplate.delete(agreementClause);	
	}

	@Override
	public List<AgreementClausesGroupMapping> fetchAllClausesGroupBasedOnAgreementType(String agreementTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementClausesGroupMapping> query = builder.createQuery(AgreementClausesGroupMapping.class);
		Root<AgreementClausesGroupMapping> rootAgreementClausesGroupMapping = query.from(AgreementClausesGroupMapping.class);
		query.where(builder.equal(rootAgreementClausesGroupMapping.get("agreementTypeCode"), agreementTypeCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String getAgreementTypeCodeByAgreementId(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT agreementTypeCode FROM AgreementHeader WHERE agreementRequestId=:agreementRequestId ";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter("agreementRequestId", agreementRequestId);
		return query.uniqueResult();
	}

	@Override
	public AgreementAttachment getAgreementAgreementAttachmentBasedOnParams(Integer agreementRequestId, Integer versionNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementAttachment> query = builder.createQuery(AgreementAttachment.class);
		Root<AgreementAttachment> rootAgreementAttachment = query.from(AgreementAttachment.class);
		Predicate predicateOne = builder.equal(rootAgreementAttachment.get("agreementRequestId"), agreementRequestId);
		Predicate predicateTwo = builder.equal(rootAgreementAttachment.get("versionNumber"), versionNumber);
		Predicate predicateThree = builder.equal(rootAgreementAttachment.get("agreementAttachmentTypeCode"), "4");
		query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		return session.createQuery(query).getSingleResult();
	}


	@Override
	public AgreementWorkflowStatus fetchAgreementWorkflowStatusByStatusCode(String workflowStatusCode) {
		return hibernateTemplate.get(AgreementWorkflowStatus.class, workflowStatusCode);
	}

	@Override
	public List<AgreementClauses> fetchAgreementClausesBasedOnParams(Integer agreementRequestId, Integer clausesGroupCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementClauses> query = builder.createQuery(AgreementClauses.class);
		Root<AgreementClauses> rootAgreementClause = query.from(AgreementClauses.class);
		Predicate predicateOne = builder.equal(rootAgreementClause.get(AGREEMENT_REQUEST_ID), agreementRequestId);
		Predicate predicateTwo = builder.equal(rootAgreementClause.get("clausesGroupCode"), clausesGroupCode);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AgreementNote> fetchAgreementNoteBasedOnAgreementId(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementNote> query = builder.createQuery(AgreementNote.class);
		Root<AgreementNote> rootAgreementNote = query.from(AgreementNote.class);
		Predicate predicateOne = builder.equal(rootAgreementNote.get(AGREEMENT_REQUEST_ID), agreementRequestId);
		query.where(builder.and(predicateOne));
		query.orderBy(builder.asc(rootAgreementNote.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}


	@Override
	public Integer fetchMaxVersionAgreementTemplateBasedOnParams(String agreementTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<AgreementTypeTemplate> rootAgreementTypeTemplate = query.from(AgreementTypeTemplate.class);
		Predicate predicateOne = builder.equal(rootAgreementTypeTemplate.get("agreementTypeCode"), agreementTypeCode);
		query.select(builder.max(rootAgreementTypeTemplate.get("versionNumber")));
		query.where(builder.and(predicateOne));
		if (session.createQuery(query).getSingleResult() != null) {
			return session.createQuery(query).getSingleResult();
		}
		return 0;
	}

	@Override
	public AgreementTypeTemplate getAgreementTypeTemplateBasedOnParams(String agreementTypeCode, Integer versionNumber) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementTypeTemplate> query = builder.createQuery(AgreementTypeTemplate.class);
		Root<AgreementTypeTemplate> rootAgreementTypeTemplate = query.from(AgreementTypeTemplate.class);
		Predicate predicateOne = builder.equal(rootAgreementTypeTemplate.get("agreementTypeCode"), agreementTypeCode);
		Predicate predicateTwo = builder.equal(rootAgreementTypeTemplate.get("versionNumber"), versionNumber);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public AgreementSponsorContact fetchAgreementSponsorContactsBasedOnId(Integer agreementSponsorContactId) {
		return hibernateTemplate.get(AgreementSponsorContact.class, agreementSponsorContactId);
	}

	@Override
	public ClausesBank saveOrUpdateClausesBank(ClausesBank clausesBank) {
		hibernateTemplate.saveOrUpdate(clausesBank);
		return clausesBank;
	}

	@Override
	public List<ClausesBank> getAllClausesBank() {
		return hibernateTemplate.loadAll(ClausesBank.class);
	}

	@Override
	public ClausesBank fetchClausesBankById(Integer clausesCode) {
		return hibernateTemplate.get(ClausesBank.class, clausesCode);
	}

	@Override
	public void deleteClausesById(ClausesBank clausesBank) {
		hibernateTemplate.delete(clausesBank);	
	}

	@Override
	public AgreementNote fetchAgreementNoteById(Integer agreementNoteId) {
		return hibernateTemplate.get(AgreementNote.class, agreementNoteId);
	}

	@Override
	public AgreementNoteAttachment fetchAgreementNoteAttachmentById(Integer agreementNoteAttachmentId) {
		return hibernateTemplate.get(AgreementNoteAttachment.class, agreementNoteAttachmentId);
	}

	@Override
	public void deleteAgreementNoteAttachment(AgreementNoteAttachment agreementNoteAttachment) {
		hibernateTemplate.delete(agreementNoteAttachment);
	}

	@Override
	public List<AgreementPeopleType> fetchAgreementPeopleTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementPeopleType> query = builder.createQuery(AgreementPeopleType.class);
		Root<AgreementPeopleType> rootAgreementPeopleType = query.from(AgreementPeopleType.class);
		query.orderBy(builder.asc(rootAgreementPeopleType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AgreementPeople saveOrUpdateAgreementPeople(AgreementPeople agreementPeople) {
		hibernateTemplate.saveOrUpdate(agreementPeople);
		return agreementPeople;
	}

	@Override
	public List<AgreementPeople> getAllAgreementPeople(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementPeople> query = builder.createQuery(AgreementPeople.class);
		Root<AgreementPeople> rootAgreementPeople = query.from(AgreementPeople.class);
		query.where(builder.equal(rootAgreementPeople.get(AGREEMENT_REQUEST_ID), agreementRequestId));
		query.orderBy(builder.desc(rootAgreementPeople.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteAgreementPeople(AgreementPeople agreementPeople) {
		hibernateTemplate.delete(agreementPeople);
	}

	@Override
	public AgreementPeople getAgreementPeople(Integer agreementPeopleId) {
		return hibernateTemplate.get(AgreementPeople.class, agreementPeopleId);
	}

	@Override
	public List<String> getSectionCodesBasedOnRights(List<String> availableRights) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<RightSectionMapping> rootRightSectionMapping = query.from(RightSectionMapping.class);
		query.where(rootRightSectionMapping.get("rights").get("rightName").in(availableRights));
		query.select(rootRightSectionMapping.get("sectionCode"));
		return (List<String>)session.createQuery(query).getResultList();
	}

	@Override
	public List<String> getFieldCodesBasedOnSectionCodes(String sectionCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<FieldSectionMapping> rootFieldSectionMapping = query.from(FieldSectionMapping.class);
		query.where(builder.equal(rootFieldSectionMapping.get("sectionCode"), sectionCode));
		query.select(rootFieldSectionMapping.get("fieldCode"));
		return (List<String>)session.createQuery(query).getResultList();
	}

	@Override
	public List<AgreementNote> fetchAgreementNoteBasedOnActionLogId(Integer actionLogId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementNote> query = builder.createQuery(AgreementNote.class);
		Root<AgreementNote> rootAgreementNote = query.from(AgreementNote.class);
		Predicate predicateOne = builder.equal(rootAgreementNote.get("actionLogId"), actionLogId);
		query.where(builder.and(predicateOne));
		query.orderBy(builder.asc(rootAgreementNote.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AgreementPlaceHolder> getAllActiveAgreementPlaceHolders() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementPlaceHolder> query = builder.createQuery(AgreementPlaceHolder.class);
		Root<AgreementPlaceHolder> rootAgreementPlaceHolder = query.from(AgreementPlaceHolder.class);
		query.where(builder.equal(rootAgreementPlaceHolder.get("isActive"), true));
		query.orderBy(builder.asc(rootAgreementPlaceHolder.get("placeHolderName")));
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List<Object[]> getQuestionnaireAttachments(Integer moduleCode, Integer subModuleCode, String moduleItemKey) {
		Query query = null;
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		query = session.createSQLQuery(DashBoardQueries.GET_QUESTIONNAIRE_ATTACHMENTS);
		query.setParameter("MODULE_ITEM_CODE", moduleCode);
		query.setParameter("MODULE_SUB_ITEM_CODE", subModuleCode);
		query.setParameter("MODULE_ITEM_KEY", moduleItemKey);
		return query.getResultList();
	}

	@Override
	public AgreementPeopleType getAgreementPeopleTypeById(Integer peopleTypeId) {
		return hibernateTemplate.get(AgreementPeopleType.class, peopleTypeId);
	}

	@Override
	public AgreementTypeHistory saveOrUpdateAgreementTypeHistory(AgreementTypeHistory agreementTypeHistory) {
		hibernateTemplate.saveOrUpdate(agreementTypeHistory);
		return agreementTypeHistory;
	}

	@Override
	public List<AgreementTypeHistory> getAgreementTypeHistory(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementTypeHistory> query = builder.createQuery(AgreementTypeHistory.class);
		Root<AgreementTypeHistory> rootAgreementType = query.from(AgreementTypeHistory.class);
		query.where(builder.equal(rootAgreementType.get(AGREEMENT_REQUEST_ID), agreementRequestId));
		query.orderBy(builder.desc(rootAgreementType.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String getAgreementName(Integer agreementRequestId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT title FROM AgreementHeader WHERE agreementRequestId=:agreementRequestId";
			@SuppressWarnings("unchecked")
			org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
			query.setParameter(agreementRequestId, agreementRequestId);
			return query.uniqueResult();
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public List<SponsorRole> fetchSponsorRoles() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<SponsorRole> query = builder.createQuery(SponsorRole.class);
		Root<SponsorRole> rootAgreementSponsorType = query.from(SponsorRole.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<AgreementSponsorType> fetchAgreementSponsorTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementSponsorType> query = builder.createQuery(AgreementSponsorType.class);
		Root<AgreementSponsorType> rootAgreementSponsorType = query.from(AgreementSponsorType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public String getPrimaryOrganisationName(Integer agreementRequestId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String maxHQLQuery = "SELECT MAX(agreementSponsorId) FROM AgreementSponsor WHERE agreementRequestId=:agreementRequestId and agreementSponsorTypeCode = 1";
			Query<Integer> countQuery = session.createQuery(maxHQLQuery);
			countQuery.setParameter("agreementRequestId", agreementRequestId);
			Integer agreementSponsorId = countQuery.uniqueResult();
			String hqlQuery = "SELECT sponsorName FROM AgreementSponsor WHERE agreementSponsorId=:agreementSponsorId";
			Query<String> query = session.createQuery(hqlQuery);
			query.setParameter("agreementSponsorId", agreementSponsorId);
			return query.uniqueResult();
		} catch (Exception e) {
			return "";
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public AgreementPeople getAgreementPI(Integer agreementRequestId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String maxHQLQuery = "SELECT MAX(agreementPeopleId) FROM AgreementPeople WHERE agreementRequestId=:agreementRequestId and peopleTypeId = 3";
			Query<Integer> countQuery = session.createQuery(maxHQLQuery);
			countQuery.setParameter("agreementRequestId", agreementRequestId);
			Integer agreementPeopleId = countQuery.uniqueResult();
			String hqlQuery = "FROM AgreementPeople WHERE agreementPeopleId=:agreementPeopleId";
			Query<AgreementPeople> query = session.createQuery(hqlQuery);
			query.setParameter("agreementPeopleId", agreementPeopleId);
			return query.uniqueResult();
		} catch (Exception e) {
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public Integer getTriageHeaderIdBasedOnAgreementId(Integer agreementHeaderId, Integer moduleCode) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String maxHQLQuery = "SELECT moduleItemKey FROM TriageHeader WHERE triageTemplateId=:triageTemplateId and moduleCode = :moduleCode";
			Query<String> countQuery = session.createQuery(maxHQLQuery);
			countQuery.setParameter("moduleItemKey", agreementHeaderId);
			countQuery.setParameter("moduleCode", moduleCode);
			return Integer.valueOf(countQuery.uniqueResult());
		} catch (Exception e) {
			return 0;
		}
	}

	@Override
	public AgreementPeople checkPersonExist(Integer agreementRequestId, String personId,Integer peopleTypeId ) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AgreementPeople> query = builder.createQuery(AgreementPeople.class);
			List<AgreementPeople> agreementPeoples = new ArrayList<>();
			Root<AgreementPeople> rootAgreementPeople = query.from(AgreementPeople.class);
			Predicate predicateOne = builder.equal(rootAgreementPeople.get(AGREEMENT_REQUEST_ID), agreementRequestId);
			Predicate predicateTwo = builder.equal(rootAgreementPeople.get("personId"), personId);
			Predicate predicateThree = builder.equal(rootAgreementPeople.get("peopleTypeId"), peopleTypeId);
			query.where(builder.and(predicateOne, predicateTwo, predicateThree));
			agreementPeoples = session.createQuery(query).getResultList();
			return !agreementPeoples.isEmpty()  ? agreementPeoples.get(0):null;
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<AgreementReviewType> fetchAgreementReviewTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementReviewType> query = builder.createQuery(AgreementReviewType.class);
		Root<AgreementReviewType> rootAgreementReviewType = query.from(AgreementReviewType.class);
		query.orderBy(builder.asc(rootAgreementReviewType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AgreementReviewType fetchAgreementReviewTypes(String agreementReviewTypeCode) {
		return hibernateTemplate.get(AgreementReviewType.class, agreementReviewTypeCode);
	}

	@Override
	public AgreementAssociationDetail saveOrUpdateAgreementAssociationDetail(AgreementAssociationDetail associationDetail) {
		hibernateTemplate.saveOrUpdate(associationDetail);
		return associationDetail;
	}

	@Override
	public AgreementAssociationLink saveOrUpdateAgreementAssociationLink(AgreementAssociationLink moduleLink) {
		hibernateTemplate.saveOrUpdate(moduleLink);
		return moduleLink;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public Boolean checkForAgreementSponsor(String sponsorCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT COUNT(*) FROM AgreementSponsor WHERE sponsorCode = :sponsorCode";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("sponsorCode", sponsorCode);
		Long count = (Long) query.getSingleResult();
		if (count > 0) {
			return true;
		}
		return false;
	}

	@Override
	public List<AgreementAssociationLink> getExternalModuleLinkDetails(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementAssociationLink> query = builder.createQuery(AgreementAssociationLink.class);
		Root<AgreementAssociationLink> rootAgreementLink = query.from(AgreementAssociationLink.class);
		query.where(builder.equal(rootAgreementLink.get("agreementRequestId"), agreementRequestId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteAgreementLink(Integer moduleCode, String moduleItemKey, Integer agreementRequestId) {
		if (moduleCode != null && agreementRequestId != null) {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<AgreementAssociationLink> query = builder.createQuery(AgreementAssociationLink.class);
			Root<AgreementAssociationLink> root = query.from(AgreementAssociationLink.class);
			Predicate predicateModuleCode = builder.equal(root.get("moduleCode"), moduleCode);
			Predicate predicateModuleItemKey = builder.equal(root.get("moduleItemKey"), moduleItemKey);
			Predicate predicateAgreementId = builder.equal(root.get("agreementRequestId"), agreementRequestId);		
			query.where(builder.and(predicateModuleCode, predicateModuleItemKey, predicateAgreementId));
			hibernateTemplate.delete(session.createQuery(query).uniqueResult());
		}
	}

	@Override
	public AgreementType fetchAgreementTypeByTypeCode(String typeCode) {
		return hibernateTemplate.get(AgreementType.class, typeCode);
	}

	@SuppressWarnings("unchecked")
	@Override
	public String getPrimaryOrganisationCode(Integer agreementRequestId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			String hqlQuery = "SELECT sponsorCode FROM AgreementSponsor WHERE agreementSponsorId IN (SELECT MAX(agreementSponsorId) FROM AgreementSponsor WHERE agreementRequestId=:agreementRequestId and agreementSponsorTypeCode = 1)";
			Query<String> query = session.createQuery(hqlQuery);
			query.setParameter("agreementRequestId", agreementRequestId);
			return query.uniqueResult();
		} catch (Exception e) {
			return "";
		}
	}

	@Override
	public Boolean checkIfExternalModuleLinked(Integer agreementRequestId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT COUNT(*) FROM AgreementAssociationLink WHERE agreementRequestId = :agreementRequestId";
		Query query = session.createQuery(hqlQuery);
		query.setParameter("agreementRequestId", agreementRequestId);
		Long count = (Long) query.getSingleResult();
		if (count > 0) {
			return true;
		}
		return false;
	}

	@Override
	public void deleteClausesGroup(Integer clausesGroupCode) {
		hibernateTemplate.delete(hibernateTemplate.get(ClausesGroup.class, clausesGroupCode));
	}

	@Override
	public void deleteAgreementClausesByClauseGroupCode(Integer clausesGroupCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AgreementClauses> query = builder.createCriteriaDelete(AgreementClauses.class);
		Root<AgreementClauses> root = query.from(AgreementClauses.class);
		query.where(builder.equal(root.get("clausesGroupCode"), clausesGroupCode));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public void deleteAgreementTypeClausesMappingByClauseGroupCode(Integer clausesGroupCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<AgreementClausesGroupMapping> query = builder.createCriteriaDelete(AgreementClausesGroupMapping.class);
		Root<AgreementClausesGroupMapping> root = query.from(AgreementClausesGroupMapping.class);
		query.where(builder.equal(root.get("clausesGroupCode"), clausesGroupCode));
		session.createQuery(query).executeUpdate();
	}

	@Override
	public List<AgreementPeople> getAgreementPeopleByTypeId(Integer agreementRequestId, Integer peopleTypeId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AgreementPeople> query = builder.createQuery(AgreementPeople.class);
		Root<AgreementPeople> root = query.from(AgreementPeople.class);
		Predicate predicateAgreementRequestId = builder.equal(root.get("agreementRequestId"), agreementRequestId);
		Predicate predicatePeopleTypeId = builder.equal(root.get("peopleTypeId"), peopleTypeId);
		query.where(builder.and(predicateAgreementRequestId, predicatePeopleTypeId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AgreementAttachmentStatus getAgreementAttachmentStatusByCode(String code) {
		return hibernateTemplate.get(AgreementAttachmentStatus.class, code);
	}

	@Override
	public AgreementSponsorType getAgreementSponsorTypeByTypeCode(String sponsorTypeCode) {
		return hibernateTemplate.get(AgreementSponsorType.class, sponsorTypeCode);
	}
	
	@Override
	public void updateAttachmentDetails(String description, Integer agreementAttachmentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder criteriaBuilder = session.getCriteriaBuilder();
		CriteriaUpdate<AgreementAttachment> criteriaUpdate = criteriaBuilder.createCriteriaUpdate(AgreementAttachment.class);
		Root<AgreementAttachment> root = criteriaUpdate.from(AgreementAttachment.class);
		criteriaUpdate.set("description", description);
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(criteriaBuilder.equal(root.get("agreementAttachmentId"),agreementAttachmentId));		 		
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	public String deleteAgreement(Integer agreementRequestId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			ProcedureCall procedure = session.createStoredProcedureCall("DELETE_AGREEMENT_RECORD");
			procedure.registerParameter(1, String.class, ParameterMode.IN).bindValue(agreementRequestId.toString());
			procedure.execute();
			return ("Agreement Deleted Successfully");
		} catch (Exception e) {
			logger.error("error occured while deleteting agreement", e);
			return ("Error in deleting agreement");
		}
	}
}
