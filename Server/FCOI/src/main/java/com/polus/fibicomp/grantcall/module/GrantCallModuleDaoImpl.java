package com.polus.fibicomp.grantcall.module;

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

import com.polus.fibicomp.grantcall.pojo.GrantCallActionLog;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCallContact;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibility;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibleDepartment;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIHeader;
import com.polus.fibicomp.grantcall.pojo.GrantCallRelevant;
import com.polus.fibicomp.grantcall.pojo.GrantCallResearchArea;
import com.polus.fibicomp.proposal.pojo.ProposalEvaluationScore;

@Transactional
@Service(value = "grantCallModuleDao")
public class GrantCallModuleDaoImpl implements GrantCallModuleDao {

	protected static Logger logger = LogManager.getLogger(GrantCallModuleDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public List<GrantCallAttachment> fetchGrantCallAttachmentBasedOnGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallAttachment> query = builder.createQuery(GrantCallAttachment.class);
		Root<GrantCallAttachment> rootGrantCallAttachment = query.from(GrantCallAttachment.class);
		query.where(builder.equal(rootGrantCallAttachment.get("grantCallId"), grantCallId));
		query.orderBy(builder.desc(rootGrantCallAttachment.get("updateTimeStamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallAttachment saveOrUpdateGrantCallAttachment(GrantCallAttachment grantCallAttachment) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallAttachment);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallAttachment: {}", e.getMessage());
		}
		return grantCallAttachment;
	}

	@Override
	public GrantCallAttachment deleteGrantCallAttachment(GrantCallAttachment grantCallAttachment) {
		try {
			hibernateTemplate.delete(grantCallAttachment);
		} catch (Exception e) {
			logger.error("Exception in deleteGrantCallAttachment: {}", e.getMessage());
		}
		return grantCallAttachment;
	}

	@Override
	public GrantCallAttachment fetchGrantCallAttachmentById(Integer attachmentId) {
		return hibernateTemplate.get(GrantCallAttachment.class, attachmentId);
	}

	@Override
	public List<GrantCallContact> fetchGrantCallContactBasedOnGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallContact> query = builder.createQuery(GrantCallContact.class);
		Root<GrantCallContact> rootGrantCallContact = query.from(GrantCallContact.class);
		query.where(builder.equal(rootGrantCallContact.get("grantCallId"), grantCallId));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallContact saveOrUpdateGrantCallContact(GrantCallContact grantCallContact) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallContact);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallContact: {}", e.getMessage());
		}
		return grantCallContact;
	}

	@Override
	public GrantCallContact deleteGrantCallContact(GrantCallContact grantCallContact) {
		try {
			hibernateTemplate.delete(grantCallContact);
		} catch (Exception e) {
			logger.error("Exception in deleteGrantCallContact: {}", e.getMessage());
		}
		return grantCallContact;
	}

	@Override
	public GrantCallContact fetchGrantCallContactById(Integer grantContactId) {
		return hibernateTemplate.get(GrantCallContact.class, grantContactId);
	}

	@Override
	public List<GrantCallResearchArea> fetchGrantCallResearchAreaBasedOnGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallResearchArea> query = builder.createQuery(GrantCallResearchArea.class);
		Root<GrantCallResearchArea> rootGrantCallResearchArea = query.from(GrantCallResearchArea.class);
		query.where(builder.equal(rootGrantCallResearchArea.get("grantCallId"), grantCallId));
		query.orderBy(builder.desc(rootGrantCallResearchArea.get("updateTimeStamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallResearchArea saveOrUpdateGrantCallResearchArea(GrantCallResearchArea grantCallResearchArea) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallResearchArea);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallResearchArea: {}", e.getMessage());
		}
		return grantCallResearchArea;
	}

	@Override
	public GrantCallResearchArea deleteGrantCallResearchArea(GrantCallResearchArea grantCallResearchArea) {
		try {
			hibernateTemplate.delete(grantCallResearchArea);
		} catch (Exception e) {
			logger.error("Exception in deleteGrantCallResearchArea: {}", e.getMessage());
		}
		return grantCallResearchArea;
	}

	@Override
	public GrantCallResearchArea fetchGrantCallResearchAreaById(Integer grantResearchAreaId) {
		return hibernateTemplate.get(GrantCallResearchArea.class, grantResearchAreaId);
	}

	@Override
	public List<GrantCallEligibility> fetchGrantCallEligibilityBasedOnGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallEligibility> query = builder.createQuery(GrantCallEligibility.class);
		Root<GrantCallEligibility> rootGrantCallEligibility = query.from(GrantCallEligibility.class);
		query.where(builder.equal(rootGrantCallEligibility.get("grantCallId"), grantCallId));
		query.orderBy(builder.desc(rootGrantCallEligibility.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallEligibility saveOrUpdateGrantCallEligibility(GrantCallEligibility grantCallEligibility) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallEligibility);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallEligibility: {}", e.getMessage());
		}
		return grantCallEligibility;
	}

	@Override
	public GrantCallEligibility deleteGrantCallEligibility(GrantCallEligibility grantCallEligibility) {
		try {
			hibernateTemplate.delete(grantCallEligibility);
		} catch (Exception e) {
			logger.error("Exception in deleteGrantCallEligibility: {}", e.getMessage());
		}
		return grantCallEligibility;
	}

	@Override
	public GrantCallEligibility fetchGrantCallEligibilityById(Integer grantEligibilityId) {
		return hibernateTemplate.get(GrantCallEligibility.class, grantEligibilityId);
	}

	@Override
	public List<GrantCallEligibleDepartment> fetchGrantCallEligibleDepartmentBasedOnGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallEligibleDepartment> query = builder.createQuery(GrantCallEligibleDepartment.class);
		Root<GrantCallEligibleDepartment> rootGrantCallEligibleDepartment = query
				.from(GrantCallEligibleDepartment.class);
		query.where(builder.equal(rootGrantCallEligibleDepartment.get("grantCallId"), grantCallId));
		query.orderBy(builder.desc(rootGrantCallEligibleDepartment.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallEligibleDepartment saveOrUpdateGrantCallEligibleDepartment(GrantCallEligibleDepartment grantCallEligibleDepartment) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallEligibleDepartment);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallEligibleDepartment: {}", e.getMessage());
		}
		return grantCallEligibleDepartment;
	}

	@Override
	public GrantCallEligibleDepartment deleteGrantCallEligibleDepartment(GrantCallEligibleDepartment grantCallEligibleDepartment) {
		try {
			hibernateTemplate.delete(grantCallEligibleDepartment);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallEligibleDepartment: {}", e.getMessage());
		}
		return grantCallEligibleDepartment;
	}

	@Override
	public GrantCallEligibleDepartment fetchGrantCallEligibleDepartmentById(Integer grantEligibilityDepartmentId) {
		return hibernateTemplate.get(GrantCallEligibleDepartment.class, grantEligibilityDepartmentId);
	}

	@Override
	public List<GrantCallRelevant> fetchGrantCallRelevantBasedOnGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallRelevant> query = builder.createQuery(GrantCallRelevant.class);
		Root<GrantCallRelevant> rootGrantCallRelevant = query.from(GrantCallRelevant.class);
		query.where(builder.equal(rootGrantCallRelevant.get("grantCallId"), grantCallId));
		query.orderBy(builder.desc(rootGrantCallRelevant.get("updateTimestamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallRelevant saveOrUpdateGrantCallRelevant(GrantCallRelevant grantCallRelevant) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallRelevant);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallRelevant: {}", e.getMessage());
		}
		return grantCallRelevant;
	}

	@Override
	public GrantCallRelevant deleteGrantCallRelevant(GrantCallRelevant grantCallRelevant) {
		try {
			hibernateTemplate.delete(grantCallRelevant);
		} catch (Exception e) {
			logger.error("Exception in deleteGrantCallRelevant: {}", e.getMessage());
		}
		return grantCallRelevant;
	}

	@Override
	public GrantCallRelevant fetchGrantCallRelevantById(Integer grantCallRelevantId) {
		return hibernateTemplate.get(GrantCallRelevant.class, grantCallRelevantId);
	}

	@Override
	public List<ProposalEvaluationScore> fetchProposalEvaluationScoreBasedOnGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<ProposalEvaluationScore> query = builder.createQuery(ProposalEvaluationScore.class);
		Root<ProposalEvaluationScore> rootProposalEvaluationScore = query.from(ProposalEvaluationScore.class);
		query.where(builder.equal(rootProposalEvaluationScore.get("grantHeaderId"), grantCallId));
		query.orderBy(builder.desc(rootProposalEvaluationScore.get("updateTimeStamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public ProposalEvaluationScore saveOrUpdateProposalEvaluationScore(ProposalEvaluationScore proposalEvaluationScore) {
		try {
			hibernateTemplate.saveOrUpdate(proposalEvaluationScore);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateProposalEvaluationScore: {}", e.getMessage());
		}
		return proposalEvaluationScore;
	}

	@Override
	public ProposalEvaluationScore deleteProposalEvaluationScore(ProposalEvaluationScore proposalEvaluationScore) {
		try {
			hibernateTemplate.delete(proposalEvaluationScore);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateProposalEvaluationScore: {}", e.getMessage());
		}
		return proposalEvaluationScore;
	}

	@Override
	public ProposalEvaluationScore fetchProposalEvaluationScoreById(Integer proposalEvalutionScoreId) {
		return hibernateTemplate.get(ProposalEvaluationScore.class, proposalEvalutionScoreId);
	}

	@Override
	public List<GrantCallIOIHeader> fetchGrantCallIOIHeaderBasedOnGrantCallId(Integer grantCallId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallIOIHeader> query = builder.createQuery(GrantCallIOIHeader.class);
		Root<GrantCallIOIHeader> rootGrantCallIOIHeader = query.from(GrantCallIOIHeader.class);
		query.where(builder.equal(rootGrantCallIOIHeader.get("grantCallId"), grantCallId));
		query.orderBy(builder.desc(rootGrantCallIOIHeader.get("updateTimeStamp")));
		return session.createQuery(query).getResultList();
	}

	@Override
	public GrantCallIOIHeader saveOrUpdateGrantCallIOIHeader(GrantCallIOIHeader grantCallIOIHeader) {
		try {
			hibernateTemplate.saveOrUpdate(grantCallIOIHeader);
		} catch (Exception e) {
			logger.error("Exception in saveOrUpdateGrantCallIOIHeader: {}", e.getMessage());
		}
		return grantCallIOIHeader;
	}

	@Override
	public GrantCallIOIHeader deleteGrantCallIOIHeader(GrantCallIOIHeader grantCallIOIHeader) {
		try {
			hibernateTemplate.delete(grantCallIOIHeader);
		} catch (Exception e) {
			logger.error("Exception in deleteGrantCallIOIHeader: {}", e.getMessage());
		}
		return grantCallIOIHeader;
	}

	@Override
	public GrantCallIOIHeader fetchGrantCallIOIHeaderById(Integer grantCallIOIId) {
		return hibernateTemplate.get(GrantCallIOIHeader.class, grantCallIOIId);
	}

	@Override
	public List<GrantCallAttachment> fetchGrantCallAttachmentBasedOnGrantCallIdAndDocumentId(Integer grantCallId, Integer documentId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCallAttachment> query = builder.createQuery(GrantCallAttachment.class);
		Root<GrantCallAttachment> rootGrantCallAttachment = query.from(GrantCallAttachment.class);
		Predicate predicateOne = builder.equal(rootGrantCallAttachment.get("grantCallId"),grantCallId);
		Predicate predicateTwo = builder.equal(rootGrantCallAttachment.get("documentId"),documentId);
		query.where(builder.and(predicateOne, predicateTwo));
		return session.createQuery(query).getResultList();
	}

	@Override
	public void deleteGrantCallActionLog(List<GrantCallActionLog> grantCallActionLogs) {
		hibernateTemplate.deleteAll(grantCallActionLogs);
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<GrantCallAttachment> fetchGrantCallAttachmentWithLastVersion(Integer grantCallId) {
		StringBuilder hqlQuery = new StringBuilder().append("SELECT att FROM GrantCallAttachment att ");
		hqlQuery.append("WHERE (att.documentId, att.versionNumber) IN " );
		hqlQuery.append("(SELECT DISTINCT attach.documentId, MAX(attach.versionNumber) ");
		hqlQuery.append("FROM GrantCallAttachment attach WHERE attach.grantCallId = :grantCallId GROUP BY attach.documentId) " );
		hqlQuery.append("AND att.grantCallId = :grantCallId");
		Query queryGrantCallAttachment = hibernateTemplate.getSessionFactory().getCurrentSession().createQuery(hqlQuery.toString());
		queryGrantCallAttachment.setParameter("grantCallId",grantCallId);
		return queryGrantCallAttachment.getResultList();	
	}

}
