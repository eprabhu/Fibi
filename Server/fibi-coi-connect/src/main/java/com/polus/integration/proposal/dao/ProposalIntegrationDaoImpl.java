package com.polus.integration.proposal.dao;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.proposal.pojo.COIIntegrationPropQuestAns;
import com.polus.integration.proposal.questionnaire.pojo.FibiCoiQnrMapping;
import com.polus.questionnaire.dto.FetchQnrAnsHeaderDto;
import com.polus.questionnaire.dto.GetQNRDetailsDto;
import com.polus.questionnaire.dto.QuestionnaireSaveDto;
import com.polus.questionnaire.service.QuestionnaireEngineServiceImpl;

import jakarta.persistence.EntityManager;
import jakarta.persistence.NoResultException;
import jakarta.persistence.Query;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Transactional
@Service
public class ProposalIntegrationDaoImpl implements ProposalIntegrationDao {

	protected static Logger logger = LogManager.getLogger(ProposalIntegrationDaoImpl.class.getName());

	@Autowired
	private EntityManager entityManager;

	@Autowired
	private QuestionnaireEngineServiceImpl questionnaireService;

	@Override
	public FibiCoiQnrMapping getQuestionnaireMappingInfo(Integer questionnaireId) {
		CriteriaBuilder builder = entityManager.getCriteriaBuilder();
        CriteriaQuery<FibiCoiQnrMapping> query = builder.createQuery(FibiCoiQnrMapping.class);
        Root<FibiCoiQnrMapping> root = query.from(FibiCoiQnrMapping.class);
        query.where(builder.equal(root.get("sourceQnrId"), questionnaireId));
		return entityManager.createQuery(query).getSingleResult();
	}

	@Override
	public String getQuestionAnswerByParams(Integer questionId, Integer questionnaireId, Integer proposalNumber, String disclosurePersonId) {
		String answer = null;
	    try {
	        CriteriaBuilder builder = entityManager.getCriteriaBuilder();
	        CriteriaQuery<String> query = builder.createQuery(String.class);
	        Root<COIIntegrationPropQuestAns> root = query.from(COIIntegrationPropQuestAns.class);
	        Predicate predicate1 = builder.equal(root.get("proposalNumber"), proposalNumber);
	        Predicate predicate2 = builder.equal(root.get("keyPersonId"), disclosurePersonId);
	        Predicate predicate3 = builder.equal(root.get("questionnaireId"), questionnaireId);
	        Predicate predicate4 = builder.equal(root.get("questionId"), questionId);
	        query.select(root.get("answer"));
	        query.where(builder.and(predicate1, predicate2, predicate3, predicate4));
	        answer = entityManager.createQuery(query).getSingleResult();
	    } catch (NoResultException e) {
	        logger.error("No answer found for the provided parameters.", e.getMessage());
	    } catch (Exception e) {
	    	logger.error("Exception in getQuestionAnswerByParams", e.getMessage());
	    }
	    return answer;
	}

	@Override
	public Integer findQuestionnaireAnsHeaderId(FetchQnrAnsHeaderDto request) {
		Integer questionnaireAnswerId = questionnaireService.findQuestionnaireAnsHeaderId(request);
		return questionnaireAnswerId != -1 ? questionnaireAnswerId : null;
	}

	@Override
	public GetQNRDetailsDto getQuestionnaireDetails(GetQNRDetailsDto questionnaireDataBus) {
		return questionnaireService.getQuestionnaireDetails(questionnaireDataBus);
	}

	@Override
	public QuestionnaireSaveDto saveQuestionnaireAnswers(QuestionnaireSaveDto questionnaireDataBus) throws Exception {
		return questionnaireService.saveQuestionnaireAnswers(questionnaireDataBus);
	}

	@Override
	public Boolean canCreateProjectDisclosure(Integer questionnaireId, String personId, String proposalNumber) {
	    try {
	        Query query = entityManager.createNativeQuery("SELECT FN_INT_CAN_CREATE_PROP_DISCL(:proposalNumber, :personId, :questionnaireId)")
	        							.setParameter("proposalNumber", proposalNumber)
	                                   .setParameter("personId", personId)
	                                   .setParameter("questionnaireId", questionnaireId);

	        Object result = query.getSingleResult();
	        if (result instanceof Number) {
	            return ((Number) result).intValue() == 1;
	        }
	        return false;
	    } catch (Exception e) {
	        e.printStackTrace();
	        return false;
	    }
	}

}
