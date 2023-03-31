package com.polus.fibicomp.award.awardprojectoutcome.dao;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAcheivements;
import com.polus.fibicomp.award.pojo.AwardAssociation;
import com.polus.fibicomp.award.pojo.AwardAssociationDetail;
import com.polus.fibicomp.award.pojo.AwardAssociationType;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPublications;
import com.polus.fibicomp.award.pojo.Publication;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "awardProjectOutcomeDao")
public class AwardProjectOutcomeDaoImpl implements AwardProjectOutcomeDao {

	protected static Logger logger = LogManager.getLogger(AwardProjectOutcomeDaoImpl.class.getName());

	private static final String AWARD_ID = "awardId";
	private static final String TITLE = "title";
	private static final String PERSON_ID = "personId";
	private static final String ROLODEX_ID = "rolodexId";
	private static final String PROPOSAL_ID = "proposalId";
	private static final String IS_PI = "isPi";

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<Publication> findPublications(CommonVO vo) {
		try {
			StringBuilder authorName = new StringBuilder();
			StringBuilder titleOfPaper = new StringBuilder();
			StringBuilder publicationType = new StringBuilder();
			StringBuilder nameOfJournal = new StringBuilder();
			if (vo.getProperty1() != null && !vo.getProperty1().equals("")) {
				authorName = authorName.append("%").append(vo.getProperty1()).append("%");
			}
			if (vo.getProperty2() != null && !vo.getProperty2().equals("")) {
				titleOfPaper = titleOfPaper.append("%").append(vo.getProperty2()).append("%");
			}
			if (vo.getProperty3() != null && !vo.getProperty3().equals("")) {
				publicationType = publicationType.append("%").append(vo.getProperty3()).append("%");
			}
			if (vo.getProperty4() != null && !vo.getProperty4().equals("")) {
				nameOfJournal = nameOfJournal.append("%").append(vo.getProperty4()).append("%");
			}
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder builder = session.getCriteriaBuilder();
			CriteriaQuery<Publication> query = builder.createQuery(Publication.class);
			Root<Publication> publication = query.from(Publication.class);
			List<Predicate> inRestrictions = new ArrayList<>();
			Predicate predicateAuthorName = builder.like(publication.get("author"), authorName.toString());
			Predicate predicateTitleOfPaper = builder.like(publication.get(TITLE), titleOfPaper.toString());
			Predicate predicatePublicationType = builder.like(publication.get("publicationType"),
					publicationType.toString());
			Predicate predicateNameOfJournal = builder.like(publication.get("nameOfJournal"),
					nameOfJournal.toString());
			if (vo.getProperty1() != null && !vo.getProperty1().isEmpty() && !vo.getProperty1().equals("")) {
				inRestrictions.add(predicateAuthorName);
			}
			if (vo.getProperty2() != null && !vo.getProperty2().isEmpty() && !vo.getProperty2().equals("")) {
				inRestrictions.add(predicateTitleOfPaper);
			}
			if (vo.getProperty3() != null && !vo.getProperty3().isEmpty() && !vo.getProperty3().equals("")) {
				inRestrictions.add(predicatePublicationType);
			}
			if (vo.getProperty4() != null && !vo.getProperty4().isEmpty() && !vo.getProperty4().equals("")) {
				inRestrictions.add(predicateNameOfJournal);
			}
			if (!inRestrictions.isEmpty()) {
				query.where(builder.and(inRestrictions.toArray(new Predicate[inRestrictions.size()])));
				query.orderBy(builder.desc(publication.get("updateTimeStamp")));
				return session.createQuery(query).getResultList();
			} else {
				return Collections.emptyList();
			}
		} catch (Exception e) {
			logger.error("Error in findPublications {}", e.getMessage());
			return Collections.emptyList();
		}
	}

	@Override
	public AwardPublications saveOrUpdateAwardPublications(AwardPublications awardPublication) {
		try {
			hibernateTemplate.saveOrUpdate(awardPublication);
		} catch (Exception e) {
			logger.error("Error occured in saveOrUpdateAwardPublications : {}", e.getMessage());
			throw new ApplicationException("Error in saveOrUpdateAwardPublications", e, Constants.JAVA_ERROR);
		}
		return awardPublication;
	}

	@Override
	public AwardPublications getAwardPublicationsBasedOnId(Integer awardPublicationId) {
		return hibernateTemplate.load(AwardPublications.class, awardPublicationId);
	}

	@Override
	public AwardPublications deleteAwardPublication(AwardPublications awardPublication) {
		try {
			hibernateTemplate.delete(awardPublication);
		} catch (Exception e) {
			logger.error("Error occured in deleteAwardPublication : {}", e.getMessage());
		}
		return awardPublication;	
	}

	@Override
	public List<AwardPublications> fetchAllAwardPublications(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardPublications> awardPublication = builder.createQuery(AwardPublications.class);
		Root<AwardPublications> rootAwardPublications = awardPublication.from(AwardPublications.class);
		Predicate awardIdPredicate = builder.equal(rootAwardPublications.get(AWARD_ID), awardId);
		awardPublication.where(builder.and(awardIdPredicate));
		return session.createQuery(awardPublication).getResultList();
	}

	@Override
	public AwardAssociation saveOrUpdateAwardAssociation(AwardAssociation awardAssociation) {
		try {
			hibernateTemplate.saveOrUpdate(awardAssociation);
		} catch (Exception e) {
			logger.error("Error occured in saveOrUpdateAwardAssociation : {}", e.getMessage());
			throw new ApplicationException("Error in saveOrUpdateAwardAssociation", e, Constants.JAVA_ERROR);
		}
		return awardAssociation;
	}

	@Override
	public AwardAssociation getAwardAssociationBasedOnId(Integer awardAssociationId) {
		return hibernateTemplate.load(AwardAssociation.class, awardAssociationId);
	}

	@Override
	public AwardAssociation deleteAwardAssociation(AwardAssociation awardAssociation) {
		try {
			hibernateTemplate.delete(awardAssociation);
		} catch (Exception e) {
			logger.error("Error occured in deleteAwardAssociation : {}", e.getMessage());
		}
		return awardAssociation;	
	}

	@Override
	public List<AwardAssociation> fetchAllAwardAssociation(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAssociation> awardAssociation = builder.createQuery(AwardAssociation.class);
		Root<AwardAssociation> rootAwardAssociation = awardAssociation.from(AwardAssociation.class);
		Predicate awardIdPredicate = builder.equal(rootAwardAssociation.get(AWARD_ID), awardId);
		awardAssociation.where(builder.and(awardIdPredicate));
		return session.createQuery(awardAssociation).getResultList();
	}

	@Override
	public AwardAcheivements saveOrUpdateAwardAcheivements(AwardAcheivements awardAcheivement) {
		try {
			hibernateTemplate.saveOrUpdate(awardAcheivement);
		} catch (Exception e) {
			logger.error("Error occured in saveOrUpdateAwardAcheivements : {}", e.getMessage());
			throw new ApplicationException("Error in saveOrUpdateAwardAcheivements", e, Constants.JAVA_ERROR);
		}
		return awardAcheivement;
	}

	@Override
	public AwardAcheivements getAwardAcheivementsBasedOnId(Integer awardAcheivementAttachId) {
		return hibernateTemplate.load(AwardAcheivements.class, awardAcheivementAttachId);
	}

	@Override
	public AwardAcheivements deleteAwardAcheivement(AwardAcheivements awardAcheivement) {
		try {
			hibernateTemplate.delete(awardAcheivement);
		} catch (Exception e) {
			logger.error("Error occured in deleteAwardAcheivement : {}", e.getMessage());
		}
		return awardAcheivement;	
	}

	@Override
	public List<AwardAcheivements> fetchAllAwardAcheivements(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAcheivements> awardAcheivement = builder.createQuery(AwardAcheivements.class);
		Root<AwardAcheivements> rootAwardAcheivements = awardAcheivement.from(AwardAcheivements.class);
		Predicate awardIdPredicate = builder.equal(rootAwardAcheivements.get(AWARD_ID), awardId);
		awardAcheivement.where(builder.and(awardIdPredicate));
		return session.createQuery(awardAcheivement).getResultList();
	}

	public List<AwardAssociationType> fetchAllAssociationTypes() {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAssociationType> query = builder.createQuery(AwardAssociationType.class);
		Root<AwardAssociationType> rootAgreementSponsorType = query.from(AwardAssociationType.class);
		query.orderBy(builder.asc(rootAgreementSponsorType.get("description")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardAssociationDetail fetchAwardAssociationDetailByAwardAssociationId(Integer awardAssociationId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardAssociationDetail> awardAssociationDetail = builder.createQuery(AwardAssociationDetail.class);
		Root<AwardAssociationDetail> rootAwardAssociationDetail = awardAssociationDetail.from(AwardAssociationDetail.class);
		awardAssociationDetail.where(builder.equal(rootAwardAssociationDetail.get("awardAssociation").get("awardAssociationId"), awardAssociationId));
		return session.createQuery(awardAssociationDetail).uniqueResult();
	}

	@Override
	public AwardAssociationDetail saveAwardAssociationDetail(AwardAssociationDetail awardAssociationDetail) {
		try {
			hibernateTemplate.saveOrUpdate(awardAssociationDetail);
		} catch (Exception e) {
			logger.error("Exception in saveAwardAssociationDetail : {}", e.getMessage());
			throw new ApplicationException("Error in saveAwardAssociationDetail", e, Constants.JAVA_ERROR);
		}
		return awardAssociationDetail;
	}

	@Override
	public List<Integer> getAwardIdBasedOnParams(List<String> personIds, List<Integer> rolodexIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<AwardPerson> rootAwardPerson = query.from(AwardPerson.class);
		List<Integer> awardIds = new ArrayList<>();
		Predicate predicateOne;
		Predicate predicateTwo;
		if (!personIds.isEmpty() && !rolodexIds.isEmpty()) {
			predicateOne = rootAwardPerson.get(PERSON_ID).in(personIds);
			predicateTwo = rootAwardPerson.get(ROLODEX_ID).in(rolodexIds);
			query.where(builder.or(predicateOne, predicateTwo));
			query.select(rootAwardPerson.get(AWARD_ID));
			awardIds = session.createQuery(query).getResultList();
		} else if (!personIds.isEmpty()) {
			predicateOne = rootAwardPerson.get(PERSON_ID).in(personIds);
			query.where(builder.or(predicateOne));
			query.select(rootAwardPerson.get(AWARD_ID));
			awardIds = session.createQuery(query).getResultList();
		} else if (!rolodexIds.isEmpty()) {
			predicateTwo = rootAwardPerson.get(ROLODEX_ID).in(rolodexIds);
			query.where(builder.or(predicateTwo));
			query.select(rootAwardPerson.get(AWARD_ID));
			awardIds = session.createQuery(query).getResultList();
		}
		return awardIds;
	}

	@Override
	public List<Integer> getProposalIdBasedOnParams(List<String> personIds, List<Integer> rolodexIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Integer> query = builder.createQuery(Integer.class);
		Root<ProposalPerson> rootProposalPerson = query.from(ProposalPerson.class);
		List<Integer> proposalIds = new ArrayList<>();
		Predicate predicateOne;
		Predicate predicateTwo;
		if (!personIds.isEmpty() && !rolodexIds.isEmpty()) {
			predicateOne = rootProposalPerson.get(PERSON_ID).in(personIds);
			predicateTwo = rootProposalPerson.get(ROLODEX_ID).in(rolodexIds);
			query.where(builder.or(predicateOne, predicateTwo));
			query.select(rootProposalPerson.get(PROPOSAL_ID));
			proposalIds = session.createQuery(query).getResultList();
		} else if (!personIds.isEmpty()) {
			predicateOne = rootProposalPerson.get(PERSON_ID).in(personIds);
			query.where(builder.or(predicateOne));
			query.select(rootProposalPerson.get(PROPOSAL_ID));
			proposalIds = session.createQuery(query).getResultList();
		} else if (!rolodexIds.isEmpty()) {
			predicateTwo = rootProposalPerson.get(ROLODEX_ID).in(rolodexIds);
			query.where(builder.or(predicateTwo));
			query.select(rootProposalPerson.get(PROPOSAL_ID));
			proposalIds = session.createQuery(query).getResultList();
		}
		return proposalIds;
	}

	@Override
	public List<Award> getAwardsBasedOnParams(List<Integer> awardIds, String searchString, String awardNumber, Timestamp awardExpirationTimestamp) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Award> query = builder.createQuery(Award.class);
		Root<Award> rootAward = query.from(Award.class);
		Predicate predicateOne = rootAward.get(AWARD_ID).in(awardIds);
		Predicate predicateTwo = builder.equal(rootAward.get("awardSequenceStatus"), Constants.AWARD_FINAL_STATUS_ACTIVE);
		Predicate predicateThree = builder.like(rootAward.get(TITLE), "%" + searchString + "%");
		Predicate predicateFour = builder.notEqual(rootAward.get("awardNumber"), awardNumber);
		Predicate predicateAwardStatus = builder.equal(rootAward.get("awardSequenceStatus"), Constants.AWARD_FINAL_STATUS_PENDING);
		Predicate predicateAwardDocumentType = builder.equal(rootAward.get("awardDocumentTypeCode"), Constants.AWARD_SETUP);
		Predicate predicateStatus = builder.and(predicateAwardStatus, predicateAwardDocumentType);
		Predicate predicateAward = builder.or(predicateTwo, predicateStatus);
		if (searchString == null && awardNumber == null && awardExpirationTimestamp == null && awardIds != null) {
			query.where(builder.and(predicateOne, predicateAward));
		} else if (searchString == null && awardNumber == null) {
			Predicate predicateDate = builder.greaterThanOrEqualTo(rootAward.get("finalExpirationDate"), awardExpirationTimestamp);
			query.where(builder.and(predicateOne, predicateAward, predicateDate));
		} else {
			query.where(builder.and(predicateOne, predicateTwo, predicateThree, predicateFour));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<Proposal> getProposalsBasedOnParams(List<Integer> proposalIds, List<Integer> proposalStatuses, String searchString) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<Proposal> query = builder.createQuery(Proposal.class);
		Root<Proposal> rootProposal = query.from(Proposal.class);
		Predicate predicateOne = rootProposal.get(PROPOSAL_ID).in(proposalIds);
		Predicate predicateFour =  builder.notEqual(rootProposal.get("documentStatusCode"), Constants.PROPOSAL_DOCUMENT_STATUS_ARCHIVE);
		Predicate predicateTwo;
		Predicate predicateThree = builder.like(rootProposal.get(TITLE), "%" + searchString + "%");
		if (searchString != null) {
			predicateTwo = builder.not(rootProposal.get("statusCode").in(proposalStatuses));
			query.where(builder.and(predicateOne, predicateTwo, predicateThree, predicateFour));
		} else if (proposalStatuses == null) {
			query.where(builder.and(predicateOne, predicateFour));
		} else {
			predicateTwo = builder.not(rootProposal.get("statusCode").in(proposalStatuses));
			query.where(builder.and(predicateOne, predicateTwo, predicateFour));
		}
		return session.createQuery(query).getResultList();
	}

	@Override
	public AwardPerson getAwardPiDetails(Integer awardId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<AwardPerson> query = builder.createQuery(AwardPerson.class);
		Root<AwardPerson> rootAwardPerson = query.from(AwardPerson.class);
		Predicate predicateOne = builder.equal(rootAwardPerson.get(AWARD_ID), awardId);
		Predicate predicateTwo = builder.equal(rootAwardPerson.get(IS_PI), true);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).uniqueResult();
	}

	public ProposalPerson getProposalPiDetails(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPerson> query = builder.createQuery(ProposalPerson.class);
		Root<ProposalPerson> rootProposalPerson = query.from(ProposalPerson.class);
		Predicate predicateOne = builder.equal(rootProposalPerson.get(PROPOSAL_ID), proposalId);
		Predicate predicateTwo = builder.equal(rootProposalPerson.get(IS_PI), true);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public Boolean getIsFileDataIdFound(String fileId) {
		try {
			Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
			CriteriaBuilder cb = session.getCriteriaBuilder();
			CriteriaQuery<Long> query = cb.createQuery(Long.class);
			Root<AwardAcheivements> root = query.from(AwardAcheivements.class);
			query.select(cb.count(root));
			query.where(cb.equal(root.get("fileDataId"), fileId));
			Long count = session.createQuery(query).getSingleResult();
			return count.intValue() > 1 ? Boolean.TRUE : Boolean.FALSE;
		} catch (Exception e) {
			return Boolean.TRUE;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<String> getPublicationTypes() {
		try {
			StringBuilder hqlQuery = new StringBuilder();
			hqlQuery.append("select distinct publicationType from Publication");
			Query query = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
			return query.getResultList();
		} catch (Exception e) {
			logger.error("Error in getPublicationTypes {}", e.getMessage());
			return Collections.emptyList();          
		}		
	}

}
