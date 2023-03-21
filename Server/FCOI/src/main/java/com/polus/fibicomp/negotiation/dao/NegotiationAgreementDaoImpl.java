package com.polus.fibicomp.negotiation.dao;

import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.negotiation.pojo.NegotiationCommentAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationLocationStatus;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivity;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivityType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAgreementType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationDetails;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachmentType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsComment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsCommentType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocationType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsPersonnelType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsStatus;
import com.polus.fibicomp.negotiation.pojo.NegotiationsWorkflowStatus;

@Transactional
@Service(value = "negotiationAgreementDaoImpl")
public class NegotiationAgreementDaoImpl implements NegotiationAgreementDao {

	protected static Logger logger = LogManager.getLogger(NegotiationAgreementDaoImpl.class.getName());

	@Value("${oracledb}")
	private String oracledb;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private CommonDao commonDao;

	private static final String NEGOTIATION_ID = "negotiationId";
	private static final String CREATE_TIMESTAMP = "createTimestamp";
	private static final String NEGOTIATION_LOCATION_ID = "negotiationLocationId";
	private static final String LOCATION_TYPE_CODE = "locationTypeCode";

	@Override
	public List<NegotiationsLocation> getLocationByNegotiationId(Integer negotiationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsLocation> query = builder.createQuery(NegotiationsLocation.class);
		Root<NegotiationsLocation> rootNegotiationsLocation = query.from(NegotiationsLocation.class);
		query.where(builder.equal(rootNegotiationsLocation.get(NEGOTIATION_ID), negotiationId));
		query.orderBy(builder.desc(rootNegotiationsLocation.get(CREATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<NegotiationsAssociationType> getNegotiationAssociationTypes() {
		return hibernateTemplate.loadAll(NegotiationsAssociationType.class);
	}

	@Override
	public List<NegotiationsStatus> getAllNegotiationsStatus() {
		return hibernateTemplate.loadAll(NegotiationsStatus.class);
	}

	@Override
	public List<NegotiationsAgreementType> getNegotiationsAgreementTypes() {
		return hibernateTemplate.loadAll(NegotiationsAgreementType.class);
	}

	@Override
	public String deleteAssociations(Integer negotiationsAssociationId) {
		hibernateTemplate.delete(hibernateTemplate.get(NegotiationsAssociationDetails.class, negotiationsAssociationId));
		return commonDao.convertObjectToJSON("SUCCESS");
	}

	@Override
	public List<NegotiationsActivityType> getNegotiationsActivityTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsActivityType> query = builder.createQuery(NegotiationsActivityType.class);
		Root<NegotiationsActivityType> rootAgreementSponsorType = query.from(NegotiationsActivityType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<NegotiationsAttachmentType> getNegotiationsAttachmentTypes() {
		return hibernateTemplate.loadAll(NegotiationsAttachmentType.class);
	}

	@Override
	public List<NegotiationsLocationType> getNegotiationsLocationTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsLocationType> query = builder.createQuery(NegotiationsLocationType.class);
		Root<NegotiationsLocationType> rootAgreementSponsorType = query.from(NegotiationsLocationType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<NegotiationsPersonnelType> getNegotiationsPersonnelTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsPersonnelType> query = builder.createQuery(NegotiationsPersonnelType.class);
		Root<NegotiationsPersonnelType> rootNegotiationsPersonnelType = query.from(NegotiationsPersonnelType.class);
		query.orderBy(builder.desc(rootNegotiationsPersonnelType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<NegotiationsWorkflowStatus> getNegotiationsWorkflowStatus() {
		return hibernateTemplate.loadAll(NegotiationsWorkflowStatus.class);
	}

	@Override
	public Negotiations saveNegotiationInfo(Negotiations negotiations) {
		hibernateTemplate.saveOrUpdate(negotiations);
		return negotiations;
	}

	@Override
	public NegotiationsLocation saveOrUpdateNegotiationLocation(NegotiationsLocation negotiationsLocation) {
		hibernateTemplate.saveOrUpdate(negotiationsLocation);
		return negotiationsLocation;
	}

	@Override
	public List<NegotiationsActivity> getNegotiationsActivityByNegotiationId(Integer negotiationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsActivity> query = builder.createQuery(NegotiationsActivity.class);
		Root<NegotiationsActivity> rootNegotiationsActivity = query.from(NegotiationsActivity.class);
		Predicate predicateOne = builder.equal(rootNegotiationsActivity.get(NEGOTIATION_ID), negotiationId);
		query.where(builder.and(predicateOne));
		query.orderBy(builder.desc(rootNegotiationsActivity.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String deleteAssociatedDetail(NegotiationsAssociationDetails associationDetail) {
		hibernateTemplate.delete(associationDetail);
		return commonDao.convertObjectToJSON("Success");
	}

	@Override
	public String deleteActivity(NegotiationsActivity negotiationsActivity) {
		hibernateTemplate.delete(negotiationsActivity);
		return commonDao.convertObjectToJSON("Negotiations Activity deleted successfully");
	}

	@Override
	public NegotiationsAttachment fetchAttachmentById(Integer negotiationsAttachmentId) {
		return hibernateTemplate.get(NegotiationsAttachment.class, negotiationsAttachmentId);
	}

	@Override
	public List<NegotiationsAttachment> getAttachmentData(Integer negotiationsActivityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsAttachment> query = builder.createQuery(NegotiationsAttachment.class);
		Root<NegotiationsAttachment> rootNegotiationsAttachment = query.from(NegotiationsAttachment.class);
		query.where(builder.equal(rootNegotiationsAttachment.get("negotiationsActivityId"), negotiationsActivityId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public NegotiationsActivity saveOrUpdateActivity(NegotiationsActivity negotiationsActivity) {
		hibernateTemplate.saveOrUpdate(negotiationsActivity);
		return negotiationsActivity;
	}

	@Override
	public NegotiationsAttachment saveAttachment(NegotiationsAttachment negotiationAttachment) {
		hibernateTemplate.saveOrUpdate(negotiationAttachment);
		return negotiationAttachment;
	}
	
	@Override
	public String deleteNegotiationAssociation(Integer negotiationsAssociationId) {
		try {
			NegotiationsAssociation negotiationsAssociation = hibernateTemplate.get(NegotiationsAssociation.class, negotiationsAssociationId);
			hibernateTemplate.delete(negotiationsAssociation);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON("success");
	}

	@Override
	public NegotiationsAssociation saveNegotiationAssociation(NegotiationsAssociation negotiationAssociation) {
		try {
			hibernateTemplate.saveOrUpdate(negotiationAssociation);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return negotiationAssociation;
	}

	@Override
	public String deleteNegotiationAssociationDetails(NegotiationsAssociationDetails negotiationsAssociationDetail) {
		try {
			hibernateTemplate.delete(negotiationsAssociationDetail);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON("Negotiations Association Details deleted successfully");
	}

	@Override
	public String saveNegotiationAssociationDetails(NegotiationsAssociationDetails negotiationAssociationDetails) {
		try {
			hibernateTemplate.saveOrUpdate(negotiationAssociationDetails);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON("success");
	}


	@Override
	public Negotiations fetchNegotiationById(Integer negotiationId) {
		return hibernateTemplate.get(Negotiations.class, negotiationId);
	}

	@Override
	public NegotiationsStatus fetchNegotiationStatusByStatusCode(String negotiationStatusCode) {
		return hibernateTemplate.get(NegotiationsStatus.class, negotiationStatusCode);
	}

	@Override
	public NegotiationsStatus fetchStatusByStatusCode(String statusCode) {
		return hibernateTemplate.get(NegotiationsStatus.class, statusCode);
	}


	@Override
	public List<NegotiationsAttachment> loadNegotiationAttachments(Integer negotiationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsAttachment> query = builder.createQuery(NegotiationsAttachment.class);
		Root<NegotiationsAttachment> rootNegotiationsAttachment = query.from(NegotiationsAttachment.class);
		Predicate predicateOne = builder.equal(rootNegotiationsAttachment.get("negotiationId"), negotiationId);
		query.where(builder.and(predicateOne));
		return session.createQuery(query).getResultList();
		
	}

	@Override
	public List<NegotiationsAttachment> fetchNegotiationAttachmentBasedOnAttachmentIds(List<Integer> attachmentIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsAttachment> query = builder.createQuery(NegotiationsAttachment.class);
		Root<NegotiationsAttachment> rootNegotiationAttachment = query.from(NegotiationsAttachment.class);
		query.where(rootNegotiationAttachment.get("negotiationsAttachmentId").in(attachmentIds));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<NegotiationsAttachment> fetchNegotiationsAttachmentBasedOnNegotiationId(Integer negotiationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsAttachment> query = builder.createQuery(NegotiationsAttachment.class);
		Root<NegotiationsAttachment> rootNegotiationAttachment = query.from(NegotiationsAttachment.class);
		Predicate predicateOne = builder.equal(rootNegotiationAttachment.get("negotiationId"), negotiationId);
		query.where(builder.and(predicateOne));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void saveOrUpdateNegotiationsAttachment(NegotiationsAttachment negotiationsAttachment) {
		hibernateTemplate.saveOrUpdate(negotiationsAttachment);
	}

	@Override
	public void deleteNegotiationAttachment(NegotiationsAttachment negotiationsAttachment) {
		hibernateTemplate.delete(negotiationsAttachment);
	}

	@Override
	public List<NegotiationsAssociationDetails> fetchNegotiationAssociationDetailsBasedOnNegotiationAssociationId(Integer negotiationsAssociationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsAssociationDetails> query = builder.createQuery(NegotiationsAssociationDetails.class);
		Root<NegotiationsAssociationDetails> rootNegotiationAssociationDetails = query.from(NegotiationsAssociationDetails.class);
		Predicate predicateOne = builder.equal(rootNegotiationAssociationDetails.get("negotiationsAssociationId"), negotiationsAssociationId);
		query.where(builder.and(predicateOne));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<NegotiationLocationStatus> getNegotiationLocationStatus() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationLocationStatus> query = builder.createQuery(NegotiationLocationStatus.class);
		Root<NegotiationLocationStatus> rootAgreementSponsorType = query.from(NegotiationLocationStatus.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public NegotiationsPersonnelType getNegotiationsPersonnelTypesById(String personnelTypeCode) {
		return hibernateTemplate.get(NegotiationsPersonnelType.class, personnelTypeCode);
	}

	@Override
	public String deleteNegotiationLocation(NegotiationsLocation negotiationLocation) {
		hibernateTemplate.delete(negotiationLocation);
		return commonDao.convertObjectToJSON("sucess");
	}

	@Override
	public NegotiationsLocation getNegotiationLocationById(Integer negotiationLocationId) {
		return hibernateTemplate.get(NegotiationsLocation.class, negotiationLocationId);
	}

	@Override
	public NegotiationsCommentType fetchNegotiationsCommentTypeById(String commentTypeCode) {
		return hibernateTemplate.get(NegotiationsCommentType.class, commentTypeCode);
	}

	@Override
	public NegotiationsComment saveOrUpdateNegotiationsComment(NegotiationsComment negotiationsComment) {
		hibernateTemplate.saveOrUpdate(negotiationsComment);
		return negotiationsComment;
	}

	@Override
	public NegotiationsLocation fetchNegotiationLocationBasedOnParams(Integer negotiationId, String locationTypeCode, String personId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<NegotiationsLocation> query = builder.createQuery(NegotiationsLocation.class);
			Root<NegotiationsLocation> rootNegotiationsLocation = query.from(NegotiationsLocation.class);
			Predicate negotiation = builder.equal(rootNegotiationsLocation.get("negotiationId"), negotiationId);
			Predicate locationType = builder.equal(rootNegotiationsLocation.get(LOCATION_TYPE_CODE), locationTypeCode);
			Predicate person = builder.equal(rootNegotiationsLocation.get("assigneePersonId"), personId);
			Predicate locationStatus = builder.equal(rootNegotiationsLocation.get("locationStatusCode"),Constants.LOCATION_INPROGRESS);
			query.where(builder.and(negotiation, locationType, person, locationStatus));
			return session.createQuery(query).uniqueResult();
		} catch (Exception e) {
			return null;
		}
	}

	@Override
	public List<NegotiationsLocation> fetchNegotiationLocationBasedOnNegotiationId(Integer negotiationId, String personId, List<String> locationStatuses) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsLocation> query = builder.createQuery(NegotiationsLocation.class);
		Root<NegotiationsLocation> rootNegotiationsLocation = query.from(NegotiationsLocation.class);
		Predicate predicateOne = builder.equal(rootNegotiationsLocation.get("negotiationId"), negotiationId);
		Predicate predicateTwo;
		Predicate predicateThree = rootNegotiationsLocation.get("locationStatusCode").in(locationStatuses);
		if (personId != null) {
			predicateTwo = builder.equal(rootNegotiationsLocation.get("assigneePersonId"), personId);
			query.where(builder.and(predicateOne, predicateTwo, predicateThree));
		} else {
			query.where(builder.and(predicateOne, predicateThree));
		}
		query.orderBy(builder.desc(rootNegotiationsLocation.get(CREATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();	
	}

	@Override
	public List<NegotiationsComment> fetchNegotiationCommentBasedOnParams(Integer negotiationLocationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsComment> query = builder.createQuery(NegotiationsComment.class);
		Root<NegotiationsComment> rootNegotiationsComment = query.from(NegotiationsComment.class);
		Predicate predicateOne = builder.equal(rootNegotiationsComment.get(NEGOTIATION_LOCATION_ID), negotiationLocationId);
		query.where(builder.and(predicateOne));
		query.orderBy(builder.desc(rootNegotiationsComment.get("updateTimestamp")));
		return session.createQuery(query).getResultList();	
	}

	@Override
	public List<NegotiationsComment> fetchNegotiationCommentByNegotiationId(Integer negotiationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsComment> query = builder.createQuery(NegotiationsComment.class);
		Root<NegotiationsComment> rootNegotiationsComment = query.from(NegotiationsComment.class);
		Predicate predicateOne = builder.equal(rootNegotiationsComment.get("negotiationId"), negotiationId);
		query.where(builder.and(predicateOne));
		return session.createQuery(query).getResultList();	
	}

	@Override
	public NegotiationLocationStatus getNegotiationLocationStatusById(String locationStatusCode) {
		return hibernateTemplate.get(NegotiationLocationStatus.class, locationStatusCode);
	}

	@Override
	public NegotiationsComment getNegotiationsCommentById(Integer negotiationCommentId) {
		return hibernateTemplate.get(NegotiationsComment.class, negotiationCommentId);
	}

	@Override
	public void deleteNegotiationsComment(NegotiationsComment negotiationComment) {
		hibernateTemplate.delete(negotiationComment);
	}

	@Override
	public NegotiationCommentAttachment fetchNegotiationCommentAttachmentById(Integer negotiationCommentAttachmentId) {
		return hibernateTemplate.get(NegotiationCommentAttachment.class, negotiationCommentAttachmentId);
	}

	@Override
	public void deleteNegotiationCommentAttachment(NegotiationCommentAttachment negotiationCommentAttachment) {
		hibernateTemplate.delete(negotiationCommentAttachment);
	}

	@Override
	public Long fetchLocationCommentCount(Integer negotiationLocationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Long> query = builder.createQuery(Long.class);
		Root<NegotiationsComment> rootNegotiationsComment = query.from(NegotiationsComment.class);
		Predicate predicateOne = builder.equal(rootNegotiationsComment.get(NEGOTIATION_LOCATION_ID), negotiationLocationId);
		query.where(builder.and(predicateOne));
		query.select(builder.count(rootNegotiationsComment));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public Boolean getUserHaveReview(Integer negotiationId, String personId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsLocation> query = builder.createQuery(NegotiationsLocation.class);
		Root<NegotiationsLocation> rootNegotiationsLocation = query.from(NegotiationsLocation.class);
		Predicate negotiation = builder.equal(rootNegotiationsLocation.get(NEGOTIATION_ID), negotiationId);
		Predicate person = builder.equal(rootNegotiationsLocation.get("assigneePersonId"), personId);
		Predicate locationStatus = rootNegotiationsLocation.get("locationStatusCode").in(Constants.LOCATION_ASSIGNED, Constants.LOCATION_INPROGRESS);
		query.where(builder.and(negotiation, person, locationStatus));
		List<NegotiationsLocation> negotiationsLocations = session.createQuery(query).getResultList();
		if (negotiationsLocations != null && !negotiationsLocations.isEmpty()) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public List<NegotiationsActivity> getNegotiationsActivityByLocationId(Integer negotiationLocationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsActivity> query = builder.createQuery(NegotiationsActivity.class);
		Root<NegotiationsActivity> rootNegotiationsActivity = query.from(NegotiationsActivity.class);
		query.where(builder.equal(rootNegotiationsActivity.get("negotiationLocationId"), negotiationLocationId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public String deleteActivityAttachments(Integer negotiationsActivityId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<NegotiationsAttachment> query = builder.createQuery(NegotiationsAttachment.class);
		Root<NegotiationsAttachment> rootNegotiationsAttachment = query.from(NegotiationsAttachment.class);
		query.where(builder.equal(rootNegotiationsAttachment.get("negotiationsActivityId"), negotiationsActivityId));
		List<NegotiationsAttachment> negotiationsAttachments = session.createQuery(query).getResultList();
		if (negotiationsAttachments != null && !negotiationsAttachments.isEmpty()) {
			hibernateTemplate.deleteAll(negotiationsAttachments);
		}
		return "sucess";
	}

	@Override
	public NegotiationsActivity getNegotiationsActivityByActivityId(Integer negotiationsActivityId) {
		return hibernateTemplate.get(NegotiationsActivity.class, negotiationsActivityId);
	}

}
