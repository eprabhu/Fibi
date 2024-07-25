package com.polus.integration.proposal.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.proposal.dto.ProposalDTO;
import com.polus.integration.proposal.vo.QuestionnaireVO;


@Transactional
@Service
public interface ProposalIntegrationService {

	/**
	 * @param proposalDTOs
	 */
	public void syncProposalDetails(ProposalDTO proposalDTO);

	/**
	 * @param quetionnaireVOs
	 */
	public void syncPersonQuestionnaireAndCreateDisclosure(List<QuestionnaireVO> quetionnaireVOs);

}
