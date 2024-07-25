package com.polus.integration.proposal.dao;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.proposal.pojo.COIIntegrationPropQuestAns;
import com.polus.integration.proposal.pojo.COIIntegrationProposal;
import com.polus.integration.proposal.pojo.COIIntegrationProposalPerson;
import com.polus.integration.proposal.questionnaire.pojo.FibiCoiQnrMapping;

import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Transactional
@Service
public class ProposalIntegrationDaoImpl implements ProposalIntegrationDao {

	protected static Logger logger = LogManager.getLogger(ProposalIntegrationDaoImpl.class.getName());

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Override
	public COIIntegrationProposal saveOrUpdateCoiIntegrationProposal(COIIntegrationProposal coiIntegrationProposal) {
		try {
			hibernateTemplate.saveOrUpdate(coiIntegrationProposal);
			return coiIntegrationProposal;
		} catch (Exception e) {
            logger.error("Exception in saveOrUpdateCoiIntegrationProposal: {}", e.getMessage(), e);
            return null;
        }
	}

	@Override
	public COIIntegrationProposalPerson saveOrUpdateCoiIntegrationProposalPerson(COIIntegrationProposalPerson coiIntegrationProposalPerson) {
		try {
			hibernateTemplate.saveOrUpdate(coiIntegrationProposalPerson);
			return coiIntegrationProposalPerson;
		} catch (Exception e) {
            logger.error("Exception in saveOrUpdateCoiIntegrationProposalPerson: {}", e.getMessage(), e);
            return null;
        }
		
	}

	@Override
	public COIIntegrationPropQuestAns saveOrUpdateCoiIntegrationQuestionnaire(COIIntegrationPropQuestAns coiIntegrationPropQuestAns) {
		try {
			hibernateTemplate.saveOrUpdate(coiIntegrationPropQuestAns);
			return coiIntegrationPropQuestAns;
		} catch (Exception e) {
            logger.error("Exception in saveOrUpdateCoiIntegrationQuestionnaire: {}", e.getMessage(), e);
            return null;
        }
	}

	@Override
	public FibiCoiQnrMapping getQuestionnaireMappingInfo(Integer questionnaireId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<FibiCoiQnrMapping> query = builder.createQuery(FibiCoiQnrMapping.class);
		Root<FibiCoiQnrMapping> rootFibiCoiQnrMapping = query.from(FibiCoiQnrMapping.class);
		query.where(builder.and(builder.equal(rootFibiCoiQnrMapping.get("sourceQnrId"), questionnaireId)));
		return session.createQuery(query).uniqueResult();
	}

	@Override
	public String getQuestionAnswerByParams(Integer questionId, Integer questionnaireId, Integer proposalNumber, String disclosurePersonId) {
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<String> query = builder.createQuery(String.class);
		Root<COIIntegrationPropQuestAns> root = query.from(COIIntegrationPropQuestAns.class);
		Predicate predicate1 = builder.equal(root.get("proposalNumber"), proposalNumber);
		Predicate predicate2 = builder.equal(root.get("keyPersonId"), disclosurePersonId);
		Predicate predicate3 = builder.equal(root.get("questionnaireId"), questionnaireId);
		Predicate predicate4 = builder.equal(root.get("questionId"), questionId);
		query.select(root.get("answer"));
		query.where(builder.and(predicate1,predicate2, predicate3, predicate4));
		return session.createQuery(query).getSingleResult();
	}

}
