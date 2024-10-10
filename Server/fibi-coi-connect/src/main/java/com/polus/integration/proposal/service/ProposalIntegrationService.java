package com.polus.integration.proposal.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.proposal.dto.DisclosureResponse;
import com.polus.integration.proposal.dto.ProposalDTO;
import com.polus.integration.proposal.vo.QuestionnaireVO;


@Transactional
@Service
public interface ProposalIntegrationService {

	/**
	 * @param proposalDTOs
	 */
	public void feedProposalDetails(ProposalDTO proposalDTO);

	/**
	 * @param quetionnaireVOs
	 */
	public void feedPersonQuestionnaireAndCreateDisclosure(List<QuestionnaireVO> quetionnaireVOs);

	public DisclosureResponse feedProposalPersonDisclosureStatus(String proposalNumber, String personId);

	public DisclosureResponse checkProposalDisclosureStatus(String proposalNumber);

	public DisclosureResponse feedDisclosureExpirationDate(String disclosureType, String personId);

}
