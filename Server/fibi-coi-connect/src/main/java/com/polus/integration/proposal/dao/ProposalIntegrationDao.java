package com.polus.integration.proposal.dao;


import org.springframework.stereotype.Service;

import com.polus.integration.proposal.questionnaire.pojo.FibiCoiQnrMapping;
import com.polus.questionnaire.dto.FetchQnrAnsHeaderDto;
import com.polus.questionnaire.dto.GetQNRDetailsDto;
import com.polus.questionnaire.dto.QuestionnaireSaveDto;

import jakarta.transaction.Transactional;

@Transactional
@Service
public interface ProposalIntegrationDao {

	/**
	 * @param questionnaireId
	 * @return
	 */
	public FibiCoiQnrMapping getQuestionnaireMappingInfo(Integer questionnaireId);

	/**
	 * @param fibiQstnId
	 * @param questionnaireId
	 * @param proposalNumber
	 * @param disclosurePersonId
	 * @return
	 */
	public String getQuestionAnswerByParams(Integer fibiQstnId, Integer questionnaireId, Integer proposalNumber, String disclosurePersonId);

	/**
	 * @param questionnaireDataBus
	 * @return
	 */
	public Integer findQuestionnaireAnsHeaderId(FetchQnrAnsHeaderDto request);

	/**
	 * @param questionnaireDataBus
	 * @return
	 */
	public GetQNRDetailsDto getQuestionnaireDetails(GetQNRDetailsDto questionnaireDataBus);

	/**
	 * @param questionnaireDataBus
	 * @return
	 */
	public QuestionnaireSaveDto saveQuestionnaireAnswers(QuestionnaireSaveDto questionnaireDataBus) throws Exception;

}
