package com.polus.fibicomp.proposal.module.dao;

import java.util.Collections;
import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import com.polus.fibicomp.proposal.pojo.*;
import com.polus.fibicomp.security.AuthenticatedUser;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.prereview.pojo.PreReview;

@Transactional
@Service(value = "proposalModuleDao")
public class ProposalModuleDaoImpl implements ProposalModuleDao {

	protected static Logger logger = LogManager.getLogger(ProposalModuleDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;
	
	@Autowired
	private CommonDao commonDao;

	private static final String PROPOSAL_ID = "proposalId";
	private static final String UPDATE_TIMESTAMP = "updateTimeStamp";

	@Override
	public List<ProposalAttachment> fetchProposalAttachmentBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalAttachment> query = builder.createQuery(ProposalAttachment.class);
		Root<ProposalAttachment> rootProposalAttachment = query.from(ProposalAttachment.class);
		query.where(builder.equal(rootProposalAttachment.get(PROPOSAL_ID), proposalId));
		query.orderBy(builder.desc(rootProposalAttachment.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<BudgetHeader> fetchBudgetHeaderBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<BudgetHeader> query = builder.createQuery(BudgetHeader.class);
		Root<BudgetHeader> rootBudgetHeader = query.from(BudgetHeader.class);
		query.where(builder.equal(rootBudgetHeader.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalKeyword> fetchProposalKeywordBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalKeyword> query = builder.createQuery(ProposalKeyword.class);
		Root<ProposalKeyword> rootProposalKeyword = query.from(ProposalKeyword.class);
		query.where(builder.equal(rootProposalKeyword.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalPerson> fetchProposalPersonBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPerson> query = builder.createQuery(ProposalPerson.class);
		Root<ProposalPerson> rootProposalPerson = query.from(ProposalPerson.class);
		query.where(builder.equal(rootProposalPerson.get(PROPOSAL_ID), proposalId));
		query.orderBy(builder.asc(rootProposalPerson.get("proposalPersonRole").get("sortId")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalIrbProtocol> fetchProposalIrbProtocolBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalIrbProtocol> query = builder.createQuery(ProposalIrbProtocol.class);
		Root<ProposalIrbProtocol> rootProposalIrbProtocol = query.from(ProposalIrbProtocol.class);
		query.where(builder.equal(rootProposalIrbProtocol.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalResearchArea> fetchProposalResearchAreaBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalResearchArea> query = builder.createQuery(ProposalResearchArea.class);
		Root<ProposalResearchArea> rootProposalResearchArea = query.from(ProposalResearchArea.class);
		query.where(builder.equal(rootProposalResearchArea.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalSponsor> fetchProposalSponsorBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalSponsor> query = builder.createQuery(ProposalSponsor.class);
		Root<ProposalSponsor> rootProposalSponsor = query.from(ProposalSponsor.class);
		query.where(builder.equal(rootProposalSponsor.get(PROPOSAL_ID), proposalId));
		query.orderBy(builder.asc(rootProposalSponsor.get("fullName")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalProjectTeam> fetchProposalProjectTeamBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalProjectTeam> query = builder.createQuery(ProposalProjectTeam.class);
		Root<ProposalProjectTeam> rootProposalProjectTeam = query.from(ProposalProjectTeam.class);
		query.where(builder.equal(rootProposalProjectTeam.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalReview> fetchProposalReviewBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalReview> query = builder.createQuery(ProposalReview.class);
		Root<ProposalReview> rootProposalReview = query.from(ProposalReview.class);
		query.where(builder.equal(rootProposalReview.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalPersonAssignedRoles> fetchProposalPersonAssignedRolesBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonAssignedRoles> query = builder.createQuery(ProposalPersonAssignedRoles.class);
		Root<ProposalPersonAssignedRoles> rootProposalPersonAssignedRoles = query.from(ProposalPersonAssignedRoles.class);
		query.where(builder.equal(rootProposalPersonAssignedRoles.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public List<ProposalSpecialReview> fetchProposalSpecialReviewBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalSpecialReview> query = builder.createQuery(ProposalSpecialReview.class);
		Root<ProposalSpecialReview> rootProposalSpecialReview = query.from(ProposalSpecialReview.class);
		query.where(builder.equal(rootProposalSpecialReview.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalAttachment saveOrUpdateProposalAttachment(ProposalAttachment proposalAttachment) {
		try {
			hibernateTemplate.saveOrUpdate(proposalAttachment);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalAttachment: {} ", e.getMessage());
		}
		return proposalAttachment;
	}

	@Override
	public ProposalAttachment deleteProposalAttachment(ProposalAttachment proposalAttachment) {
		try {
			hibernateTemplate.delete(proposalAttachment);
		} catch (Exception e) {
			logger.error("exception in deleteProposalAttachment: {} ", e.getMessage());
		}
		return proposalAttachment;
	}

	@Override
	public ProposalAttachment fetchProposalAttachment(Integer attachmentId) {
		return hibernateTemplate.get(ProposalAttachment.class, attachmentId);
	}

	@Override
	public ProposalResearchArea saveOrUpdateProposalResearchArea(ProposalResearchArea proposalResearchArea) {
		try {
			hibernateTemplate.saveOrUpdate(proposalResearchArea);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalResearchArea: {} ", e.getMessage());
		}
		return proposalResearchArea;
	}

	@Override
	public ProposalResearchArea deleteProposalResearchArea(ProposalResearchArea proposalResearchArea) {
		try {
			hibernateTemplate.delete(proposalResearchArea);
		} catch (Exception e) {
			logger.error("exception in deleteProposalResearchArea: {} ", e.getMessage());
		}
		return proposalResearchArea;
	}

	@Override
	public ProposalResearchArea fetchProposalResearchArea(Integer researchAreaId) {
		return hibernateTemplate.get(ProposalResearchArea.class, researchAreaId);
	}

	@Override
	public ProposalPerson saveOrUpdateProposalPerson(ProposalPerson proposalPerson) {
		try {
			hibernateTemplate.saveOrUpdate(proposalPerson);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalPerson: {} ", e.getMessage());
		}
		return proposalPerson;
	}

	@Override
	public ProposalPerson deleteProposalPerson(ProposalPerson proposalPerson) {
		try {
			hibernateTemplate.delete(proposalPerson);
		} catch (Exception e) {
			logger.error("exception in deleteProposalPerson: {} ", e.getMessage());
		}
		return proposalPerson;
	}

	@Override
	public ProposalPerson fetchProposalPerson(Integer proposalPersonId) {
		return hibernateTemplate.get(ProposalPerson.class, proposalPersonId);
	}

	@Override
	public ProposalSponsor saveOrUpdateProposalSponsor(ProposalSponsor proposalSponsor) {
		try {
			hibernateTemplate.saveOrUpdate(proposalSponsor);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalSponsor: {} ", e.getMessage());
		}
		return proposalSponsor;
	}

	@Override
	public ProposalSponsor deleteProposalSponsor(ProposalSponsor proposalSponsor) {
		try {
			hibernateTemplate.delete(proposalSponsor);
		} catch (Exception e) {
			logger.error("exception in deleteProposalSponsor: {} ", e.getMessage());
		}
		return proposalSponsor;
	}

	@Override
	public ProposalSponsor fetchProposalSponsor(Integer sponsorId) {
		return hibernateTemplate.get(ProposalSponsor.class, sponsorId);
	}

	@Override
	public ProposalIrbProtocol saveOrUpdateProposalIrbProtocol(ProposalIrbProtocol proposalIrbProtocol) {
		try {
			hibernateTemplate.saveOrUpdate(proposalIrbProtocol);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalIrbProtocol: {} ", e.getMessage());
		}
		return proposalIrbProtocol;
	}

	@Override
	public ProposalIrbProtocol deleteProposalIrbProtocol(ProposalIrbProtocol proposalIrbProtocol) {
		try {
			hibernateTemplate.delete(proposalIrbProtocol);
		} catch (Exception e) {
			logger.error("exception in deleteProposalIrbProtocol: {} ", e.getMessage());
		}
		return proposalIrbProtocol;
	}

	@Override
	public ProposalIrbProtocol fetchProposalIrbProtocol(Integer irbProtocolId) {
		return hibernateTemplate.get(ProposalIrbProtocol.class, irbProtocolId);
	}

	@Override
	public ProposalSpecialReview saveOrUpdateProposalSpecialReview(ProposalSpecialReview proposalSpecialReview) {
		try {
			hibernateTemplate.saveOrUpdate(proposalSpecialReview);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalSpecialReview: {} ", e.getMessage());
		}
		return proposalSpecialReview;
	}

	@Override
	public ProposalSpecialReview deleteProposalSpecialReview(ProposalSpecialReview proposalSpecialReview) {
		try {
			hibernateTemplate.delete(proposalSpecialReview);
		} catch (Exception e) {
			logger.error("exception in deleteProposalSpecialReview: {} ", e.getMessage());
		}
		return proposalSpecialReview;
	}

	@Override
	public BudgetHeader deleteBudgetHeader(BudgetHeader budgetHeader) {
		try {
			hibernateTemplate.delete(budgetHeader);
		} catch (Exception e) {
			logger.error("exception in deleteBudgetHeader: {} ", e.getMessage());
		}
		return budgetHeader;
	}

	@Override
	public ProposalSpecialReview fetchProposalSpecialReview(Integer id) {
		return hibernateTemplate.get(ProposalSpecialReview.class, id);
	}

	@Override
	public ProposalReview saveOrUpdateProposalReview(ProposalReview proposalReview) {
		try {
			hibernateTemplate.saveOrUpdate(proposalReview);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalReview: {} ", e.getMessage());
		}
		return proposalReview;
	}

	@Override
	public ProposalReview deleteProposalReview(ProposalReview proposalReview) {
		try {
			hibernateTemplate.delete(proposalReview);
		} catch (Exception e) {
			logger.error("exception in deleteProposalReview: {} ", e.getMessage());
		}
		return proposalReview;
	}

	@Override
	public ProposalKPI deleteProposalKPI(ProposalKPI proposalKPI) {
		try {
			hibernateTemplate.delete(proposalKPI);
		} catch (Exception e) {
			logger.error("exception in deleteProposalKPI: {} ", e.getMessage());
		}
		return proposalKPI;
	}


	@Override
	public ProposalReview fetchProposalReview(Integer reviewId) {
		return hibernateTemplate.get(ProposalReview.class, reviewId);
	}

	@Override
	public ProposalPersonAssignedRoles saveOrUpdateProposalPersonAssignedRoles(
			ProposalPersonAssignedRoles proposalPersonAssignedRoles) {
		try {
			hibernateTemplate.saveOrUpdate(proposalPersonAssignedRoles);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalPersonAssignedRoles: {} ", e.getMessage());
		}
		return proposalPersonAssignedRoles;
	}

	@Override
	public ProposalPersonAssignedRoles deleteProposalPersonAssignedRoles(
			ProposalPersonAssignedRoles proposalPersonAssignedRoles) {
		try {
			hibernateTemplate.delete(proposalPersonAssignedRoles);
		} catch (Exception e) {
			logger.error("exception in deleteProposalPersonAssignedRoles: {} ", e.getMessage());
		}
		return proposalPersonAssignedRoles;
	}

	@Override
	public ProposalPersonAssignedRoles fetchProposalPersonAssignedRoles(Integer personRoleId) {
		return hibernateTemplate.get(ProposalPersonAssignedRoles.class, personRoleId);
	}

	@Override
	public ProposalProjectTeam saveOrUpdateProposalProjectTeam(ProposalProjectTeam proposalProjectTeam) {
		try {
			hibernateTemplate.saveOrUpdate(proposalProjectTeam);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalProjectTeam: {} ", e.getMessage());
		}
		return proposalProjectTeam;
	}

	@Override
	public List<ProposalAttachment> fetchProposalAttachmentBasedOnAttachmentIds(List<Integer> attachmentIds) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalAttachment> query = builder.createQuery(ProposalAttachment.class);
		Root<ProposalAttachment> rootProposalAttachment = query.from(ProposalAttachment.class);
		query.where(rootProposalAttachment.get("attachmentId").in(attachmentIds));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalProjectTeam deleteProposalProjectTeam(ProposalProjectTeam proposalProjectTeam) {
		try {
			hibernateTemplate.delete(proposalProjectTeam);
		} catch (Exception e) {
			logger.error("exception in deleteProposalProjectTeam: {} ", e.getMessage());
		}
		return proposalProjectTeam;
	}

	@Override
	public List<ProposalAttachment> fetchProposalAttachmentBasedOnProposalIdAndDocumentId(Integer proposalId, Integer documentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalAttachment> query = builder.createQuery(ProposalAttachment.class);
		Root<ProposalAttachment> rootProposalAttachment = query.from(ProposalAttachment.class);
		Predicate predicateProposalId = builder.equal(rootProposalAttachment.get(PROPOSAL_ID), proposalId);
		Predicate predicateDocumentId = builder.equal(rootProposalAttachment.get("documentId"), documentId);
		query.where(builder.and(predicateProposalId, predicateDocumentId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalPersonUnit deleteProposalPersonUnit(ProposalPersonUnit proposalPersonUnit) {
		try {
			hibernateTemplate.delete(proposalPersonUnit);
		} catch (Exception e) {
			logger.error("exception in deleteProposalPersonUnit: {} ", e.getMessage());
		}
		return proposalPersonUnit;
	}

	@Override
	public ProposalMileStone fetchProposalMileStone(Integer proposalMileStoneId) {
		return hibernateTemplate.get(ProposalMileStone.class, proposalMileStoneId);
	}

	@Override
	public ProposalMileStone saveOrUpdateProposalMileStone(ProposalMileStone proposalMileStone) {
		try {
			hibernateTemplate.saveOrUpdate(proposalMileStone);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalMileStone: {} ", e.getMessage());
		}
		return proposalMileStone;
	}

	@Override
	public List<ProposalMileStone> fetchProposalMileStonesBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalMileStone> query = builder.createQuery(ProposalMileStone.class);
		Root<ProposalMileStone> rootProposalMileStone = query.from(ProposalMileStone.class);
		query.where(builder.equal(rootProposalMileStone.get(PROPOSAL_ID), proposalId));
		query.orderBy(builder.desc(rootProposalMileStone.get("startMonth")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalMileStone deleteProposalMileStone(ProposalMileStone proposalMileStone) {
		try {
			hibernateTemplate.delete(proposalMileStone);
		} catch (Exception e) {
			logger.error("exception in deleteProposalMileStone: {} ", e.getMessage());
		}
		return proposalMileStone;
	}

	@Override
	public List<ProposalKPI> fetchProposalKPIBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalKPI> query = builder.createQuery(ProposalKPI.class);
		Root<ProposalKPI> rootProposalKPI = query.from(ProposalKPI.class);
		query.where(builder.equal(rootProposalKPI.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ActivityType fetchActivityTypeBasedInId(String activityTypeCode) {
		return hibernateTemplate.get(ActivityType.class, activityTypeCode);
	}

	@Override
	public List<ProposalPersonRoles> fetchProposalPersonRolesBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonRoles> query = builder.createQuery(ProposalPersonRoles.class);
		Root<ProposalPersonRoles> rootProposalSpecialReview = query.from(ProposalPersonRoles.class);
		query.where(builder.equal(rootProposalSpecialReview.get(PROPOSAL_ID), proposalId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalPersonRoles deleteProposalPersonRoles(ProposalPersonRoles proposalPersonRoles) {
		try {
			hibernateTemplate.delete(proposalPersonRoles);
		} catch (Exception e) {
			logger.error("exception in deleteProposalPersonRoles: {} ", e.getMessage());
		}
		return proposalPersonRoles;
	}

	@Override
	public List<PreReview> fetchPreReviewBasedOnProposalId(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<PreReview> query = builder.createQuery(PreReview.class);
		Root<PreReview> rootProposalSpecialReview = query.from(PreReview.class);
		Predicate predicateModuleItemKey = builder.equal(rootProposalSpecialReview.get("moduleItemKey"), proposalId.toString());
		Predicate predicateModuleItemCode = builder.equal(rootProposalSpecialReview.get("moduleItemCode"), Constants.DEV_PROPOSAL_MODULE_CODE);
		query.where(builder.and(predicateModuleItemKey, predicateModuleItemCode));
		return session.createQuery(query).getResultList();
	}

	@Override
	public PreReview deletePreReview(PreReview preReview) {
		try {
			hibernateTemplate.delete(preReview);
		} catch (Exception e) {
			logger.error("exception in deletePreReview: {} ", e.getMessage());
		}
		return preReview;
	}

	public String getPrincipalInvestigator(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		String hqlQuery = "SELECT personId FROM ProposalPerson WHERE proposalId=:proposalId AND isPi = 'Y'";
		@SuppressWarnings("unchecked")
		org.hibernate.query.Query<String> query = session.createQuery(hqlQuery);
		query.setParameter(PROPOSAL_ID, proposalId);
		return query.uniqueResult();
	}

	@Override
	public ProposalPersonUnit saveOrUpdateProposalPersonUnit(ProposalPersonUnit proposalPersonUnit) {
		try {
			hibernateTemplate.saveOrUpdate(proposalPersonUnit);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalPersonUnit: {} ", e.getMessage());
		}
		return proposalPersonUnit;
	}

	@Override
	public ProposalFundingStatus fetchProposalFundingStatusById(Integer fundingStatusCode) {
		return hibernateTemplate.get(ProposalFundingStatus.class, fundingStatusCode);
	}

	@Override
	public SponsorType fetchSponsorTypeById(String sponsorTypeCode) {
		return hibernateTemplate.get(SponsorType.class, sponsorTypeCode);
	}

	@Override
	public String getPrincipalInvestigatorName(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<ProposalPerson> rootProposalPerson = query.from(ProposalPerson.class);
		Predicate predicateProposalId = builder.equal(rootProposalPerson.get("proposalId"), proposalId.toString());
		Predicate predicateRoleId = builder.equal(rootProposalPerson.get("personRoleId"), Constants.PI_ROLE_CODE);
		query.where(builder.and(predicateProposalId, predicateRoleId));
		query.select(rootProposalPerson.get("fullName")).distinct(true);
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public ProposalPersonAttachment saveOrUpdateProposalPersonAttachment(ProposalPersonAttachment personAttachment) {
		try {
			hibernateTemplate.saveOrUpdate(personAttachment);
		} catch (Exception e) {
			logger.error("exception in saveOrUpdateProposalPersonAttachment: {} ", e.getMessage());
		}
		return personAttachment;
	}

	@Override
	public ProposalPersonAttachment getProposalPersonAttachmentById(Integer attachmentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonAttachment> query = builder.createQuery(ProposalPersonAttachment.class);
		Root<ProposalPersonAttachment> rootProposalPerson = query.from(ProposalPersonAttachment.class);
		Predicate predicateProposalId = builder.equal(rootProposalPerson.get("attachmentId"), attachmentId);
		query.where(builder.and(predicateProposalId));
		return session.createQuery(query).getSingleResult();
	}

	@Override
	public void updateProposalkeyPersonAttachment(Integer attachmentId, String description,
			Integer attachmentTypeCode) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder cb = session.getCriteriaBuilder();
		CriteriaUpdate<ProposalPersonAttachment> criteriaUpdate = cb.createCriteriaUpdate(ProposalPersonAttachment.class);
		Root<ProposalPersonAttachment> rootProposalPersonAttachment = criteriaUpdate.from(ProposalPersonAttachment.class);
		criteriaUpdate.set("description", description);
		criteriaUpdate.set("attachmentTypeCode", attachmentTypeCode);
		criteriaUpdate.set("updateTimestamp", commonDao.getCurrentTimestamp());
		criteriaUpdate.set("updateUser", AuthenticatedUser.getLoginUserName());
		criteriaUpdate.where(cb.equal(rootProposalPersonAttachment.get("attachmentId"),attachmentId));		 		
		session.createQuery(criteriaUpdate).executeUpdate();
	}

	@Override
	@Transactional(readOnly = true)
	public List<ProposalAttachment> fetchProposalAttachmentByProposalIdWithLastVersion(Integer proposalId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		StringBuilder query = new StringBuilder("SELECT att FROM ProposalAttachment att ");
		query.append("WHERE (att.documentId, att.versionNumber) IN " );
		query.append("(SELECT DISTINCT attach.documentId, MAX(attach.versionNumber) ");
		query.append("FROM ProposalAttachment attach WHERE attach.proposalId = :proposalId GROUP BY attach.documentId) " );
		query.append("AND att.proposalId = :proposalId");
		List<ProposalAttachment> dataList = session.createQuery(query.toString())
				.setParameter("proposalId", proposalId).getResultList();
		if(dataList == null || dataList.isEmpty())
			return Collections.emptyList();
		return dataList;
  }
  
	@Override
	public void addProposalPersonDegree(ProposalPersonDegree proposalPersonDegree) {
		hibernateTemplate.saveOrUpdate(proposalPersonDegree);
	}

	@Override
	public List<ProposalPersonDegree> getPersonDegree(Integer proposalPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalPersonDegree> query = builder.createQuery(ProposalPersonDegree.class);
		Root<ProposalPersonDegree> rootExternalReviewerExt = query.from(ProposalPersonDegree.class);
		query.where(builder.equal(rootExternalReviewerExt.get("proposalPersonId"), proposalPersonId));
		query.orderBy(builder.desc(rootExternalReviewerExt.get(UPDATE_TIMESTAMP)));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteProposalPersonDegree(Integer proposalPersonDegreeId) {
		hibernateTemplate.delete(hibernateTemplate.get(ProposalPersonDegree.class, proposalPersonDegreeId));
	}

	@Override
	public void deleteDegreeByProposalPersonId(Integer proposalPersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaDelete<ProposalPersonDegree> delete = builder.createCriteriaDelete(ProposalPersonDegree.class);
		Root<ProposalPersonDegree> root = delete.from(ProposalPersonDegree.class);
		delete.where(builder.equal(root.get("proposalPersonId"), proposalPersonId));
		session.createQuery(delete).executeUpdate();
	}
  
}
