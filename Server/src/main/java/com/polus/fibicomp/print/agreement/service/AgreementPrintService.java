package com.polus.fibicomp.print.agreement.service;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;

import com.polus.fibicomp.agreements.vo.AgreementVO;

public interface AgreementPrintService {

//	/**
//	 * This method is used to generate questionnaire report.
//	 * @param proposalId - proposalId.
//	 * @param personId - personId.
//	 * @param userName - userName.
//	 * @param questionnaireId - questionnaireId.
//	 * @param isSingleQuestionnairePrint - isSingleQuestionnairePrint
//	 * @return questionnaire report data.
//	 */
//	public ResponseEntity<byte[]> generateQuestionnaireReport(Integer proposalId, String personId, String userName, Integer questionnaireId, Boolean isSingleQuestionnairePrint);

	public byte[] getTemplateData(String templateTypeCode);

	/**
	 * This method is used to generate agreement report.
	 * @param agreementVO - agreementVO
	 * @return agreementVO object.
	 */
	public String generateAgreementReport(AgreementVO agreementVO, HttpServletResponse response);

	/**
	 * This method is used to merge placeholders of agreement.
	 * @param byteArray - byteArray
	 * @param agreementRequestId - agreementRequestId
	 * @return byteArray.
	 */
	public byte[] mergePlaceHoldersOfAgreement(byte[] byteArray, Integer agreementRequestId);

	/**
	 * This method is used to merge preview document.
	 * @param agreementVO - object of AgreementVO
	 * @return byteArray.
	 */
	public ResponseEntity<byte[]> previewAgreementDocument(AgreementVO agreementVO);

	/**
	 * This method is used to generate questionnaire report.
	 * @param agreementRequestId - agreementRequestId.
	 * @param personId - personId.
	 * @param userName - userName.
	 * @param questionnaireId - questionnaireId.
	 * @return Agreement report data.
	 */
	public ResponseEntity<byte[]> generateAgreementSummary(HttpServletResponse response, Integer agreementRequestId,
			String personId, String userName);

	/**
	 * This method is used to getPrintLetterTemplate
	 * @param templateTypeCode - String value of templateTypeCode
	 * @return byteArray.
	 */
	public byte[] getPrintLetterTemplate(String templateTypeCode);

	/**
	 * This method is used to merge placeholders of agreement.
	 * @param byteArray - byteArray
	 * @param agreementRequestId - agreementRequestId
	 * @param personId - personId
	 * @param userName - userName
	 * @return byteArray.
	 */
	public byte[] setMergePlaceHoldersOfAgreementSummary(byte[] data, Integer agreementRequestId, String personId, String userName);
}
