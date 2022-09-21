package com.polus.fibicomp.questionnaire.print.service;

import java.io.ByteArrayInputStream;
import java.text.ParseException;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.itextpdf.text.DocumentException;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;

@Transactional
@Service(value = "questionnairePrintService")
public interface QuestionnairePrintService {

	/**
	 * This method is used to generate questionnaire in PDF format.
	 * @param proposalId - Id of the proposal.
	 * @return - budget in PDF format.
	 * @throws DocumentException, ParseException
	 */
	  public ByteArrayInputStream questionnairePdfReport(List<QuestionnaireDataBus> questionnaireList, Proposal proposal) throws DocumentException, ParseException;

}
