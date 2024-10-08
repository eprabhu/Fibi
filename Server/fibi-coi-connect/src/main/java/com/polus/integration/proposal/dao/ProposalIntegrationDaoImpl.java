package com.polus.integration.proposal.dao;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.proposal.dto.DisclosureResponse;
import com.polus.integration.proposal.pojo.COIIntegrationPropQuestAns;
import com.polus.integration.proposal.pojo.COIIntegrationProposal;
import com.polus.integration.proposal.pojo.COIIntegrationProposalPerson;
import com.polus.integration.proposal.questionnaire.pojo.FibiCoiQnrMapping;
import com.polus.integration.proposal.repository.ProposalIntegrationRepository;
import com.polus.integration.proposal.repository.ProposalPersonIntegrationRepository;
import com.polus.integration.proposal.repository.ProposalQnAIntegrationRepository;
import com.polus.questionnaire.dto.FetchQnrAnsHeaderDto;
import com.polus.questionnaire.dto.GetQNRDetailsDto;
import com.polus.questionnaire.dto.QuestionnaireSaveDto;
import com.polus.questionnaire.service.QuestionnaireEngineServiceImpl;

import jakarta.persistence.EntityManager;
import jakarta.persistence.NoResultException;
import jakarta.persistence.PersistenceException;
import jakarta.persistence.Query;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import lombok.extern.slf4j.Slf4j;

@Transactional
@Service
@Slf4j
public class ProposalIntegrationDaoImpl implements ProposalIntegrationDao {

	@Autowired
	private EntityManager entityManager;

	@Autowired
	private QuestionnaireEngineServiceImpl questionnaireService;
	
	@Autowired
	private ProposalIntegrationRepository proposalIntegrationRepository;
	
	@Autowired
	private ProposalPersonIntegrationRepository proposalPersonIntegrationRepository;

	@Autowired
	private ProposalQnAIntegrationRepository qnAIntegrationRepository;

	@Autowired
	private JdbcTemplate jdbcTemplate;

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
	        log.error("No answer found for the provided parameters.", e.getMessage());
	    } catch (Exception e) {
	    	log.error("Exception in getQuestionAnswerByParams", e.getMessage());
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
	        log.error("Error in canCreateProjectDisclsoure", e.getMessage());
	        return false;
	    }
	}
	
	@Override
	public void saveProposal(COIIntegrationProposal coiIntegrationProposal) {
		 proposalIntegrationRepository.save(coiIntegrationProposal);
	}

	@Override
	public void saveProposalPerson(COIIntegrationProposalPerson proposalPerson) throws Exception {
		proposalPersonIntegrationRepository.save(proposalPerson);
		
	}

	@Override
	public void saveQuestionnaireAnswer(COIIntegrationPropQuestAns integrationPropQuestAns) {
		qnAIntegrationRepository.save(integrationPropQuestAns);
	}

	@Override
	public Boolean canMarkDisclosureAsVoid(Integer questionnaireId, String personId, String moduleItemId) {
		try {
	        Query query = entityManager.createNativeQuery("SELECT FN_INT_CAN_MARK_DISCL_VOID(:proposalNumber, :personId, :questionnaireId)")
	        							.setParameter("proposalNumber", moduleItemId)
	                                   .setParameter("personId", personId)
	                                   .setParameter("questionnaireId", questionnaireId);

	        Object result = query.getSingleResult();
	        if (result instanceof Number) {
	            return ((Number) result).intValue() == 1;
	        }
	        return Boolean.FALSE;
	    } catch (Exception e) {
	        log.error("Error in canMarkDisclosureAsVoid", e.getMessage());
	        return Boolean.FALSE;
	    }
	}

	@Override
	public DisclosureResponse feedProposalDisclosureStatus(String proposalNumber, String personId) {
		if (StringUtils.isBlank(proposalNumber) || StringUtils.isBlank(personId)) {
			log.warn("Invalid proposal number or person ID provided.");
			return DisclosureResponse.builder().error("Invalid proposal number or person ID.").build();
		}

		try {
			log.info("Calling stored procedure COI_INT_PROP_PERSON_DISCL_STATUS with proposalNumber: {} and personId: {}", proposalNumber, personId);

			return jdbcTemplate.execute((Connection conn) -> {
				try (CallableStatement cs = conn.prepareCall("{call COI_INT_PROP_PERSON_DISCL_STATUS(?, ?)}")) {
					cs.setString(1, proposalNumber);
					cs.setString(2, personId);

					try (ResultSet rset = cs.executeQuery()) {
						if (rset != null && rset.next()) {
							Integer id = rset.getInt("DISCLOSURE_ID");
							String status = rset.getString("DISCLOSURE_STATUS");

							log.info("Disclosure ID: {}, Status: {} for proposalNumber: {}, personId: {}", id, status, proposalNumber, personId);

							if (status != null) {
								return DisclosureResponse.builder().disclosureId(id).disclosureStatus(status).build();
							} else {
								log.warn("No disclosure data found for proposalNumber: {} and personId: {}", proposalNumber, personId);
								return DisclosureResponse.builder().message("No data found!").build();
							}
						} else {
							log.warn("ResultSet is empty for proposalNumber: {} and personId: {}", proposalNumber, personId);
							return DisclosureResponse.builder().message("No data found!").build();
						}
					}
				} catch (SQLException ex) {
					log.error("SQLException while executing stored procedure COI_INT_PROP_PERSON_DISCL_STATUS for proposalNumber: {}, personId: {}: {}", proposalNumber, personId, ex.getMessage(), ex);
					throw new RuntimeException("A SQL error occurred during the procedure call.", ex);
				}
			});
		} catch (DataAccessException e) {
			log.error("DataAccessException while executing stored procedure COI_INT_PROP_PERSON_DISCL_STATUS with proposalNumber: {} and personId: {}: {}", proposalNumber, personId, e.getMessage(), e);
			return DisclosureResponse.builder().error("A SQL error occurred while fetching the disclosure status. Please try again later.").build();
		} catch (Exception e) {
			log.error("Unexpected error occurred while fetching person disclosure details for proposalNumber: {} and personId: {}: {}", proposalNumber, personId, e.getMessage(), e);
			return DisclosureResponse.builder().error("An error occurred while fetching the disclosure status. Please try again later.").build();
		}
	}

	@Override
	public DisclosureResponse checkProposalDisclosureStatus(String proposalNumber) {
		if (StringUtils.isBlank(proposalNumber)) {
			log.warn("Invalid proposal number provided.");
			return DisclosureResponse.builder().error("Invalid proposal number.").build();
		}

		try {
			log.info("Calling database function COI_INT_PROP_DISCL_STATUS with proposalNumber: {}", proposalNumber);
			Query query = entityManager.createNativeQuery("SELECT COI_INT_PROP_DISCL_STATUS(:proposalNumber)").setParameter("proposalNumber", proposalNumber);

			Object result = query.getSingleResult();
			log.info("Function result for proposalNumber {}: {}", proposalNumber, result);

			if (result instanceof Number) {
				Integer disclosureSubmitted = ((Number) result).intValue();
				String message = (disclosureSubmitted == 1) ? "Disclosure Submitted." : "Disclosure Not Submitted.";
				log.info("Proposal {} - {}", proposalNumber, message);
				return DisclosureResponse.builder().disclosureSubmitted(disclosureSubmitted).message(message).build();
			}

			log.warn("Unexpected result type from function for proposalNumber {}: {}", proposalNumber, result);
			return DisclosureResponse.builder().error("Unexpected result from the database.").build();

		} catch (PersistenceException e) {
			log.error("Database error while fetching disclosure status for proposalNumber {}: {}", proposalNumber, e.getMessage(), e);
			return DisclosureResponse.builder().error("A database error occurred. Please try again later.").build();
		} catch (Exception e) {
			log.error("Unexpected error while fetching disclosure status for proposalNumber {}: {}", proposalNumber, e.getMessage(), e);
			return DisclosureResponse.builder().error("An unexpected error occurred. Please try again later.").build();
		}
	}

}
