package com.polus.fibicomp.agreements.service;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.agreements.dto.AgreementComment;
import com.polus.fibicomp.agreements.pojo.AgreementNote;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.negotiation.pojo.NegotiationsComment;

@Service
public interface AgreementCommentService {

	/**
	 * This method is used to add Agreement Comment
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	//public String addAgreementComment(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to add Location Review Comment
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	//public String addLocationReviewComment(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to add location Comment
	 * @param agreementVO 
	 * @return String response.
	 */
	//public String addLocationComment(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used delete Agreement Comment
	 * @param agreementVO 
	 * @return String response.
	 */
	//public String deleteAgreementComment(AgreementVO vo);

	/**
	 * This method is used to loadReviewComments
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	//public String loadReviewComments(AgreementVO vo);

	/**
	 * This method is used prepareLocationComments
	 * @param List of NegotiationsComment 
	 * @return List of NegotiationsComment.
	 */
	//public List<NegotiationsComment> prepareLocationComments(List<NegotiationsComment> negotiationsComments);

	/**
	 * This method is used prepareAgreementComment
	 * @param agreementRequestId 
	 * @return List of AgreementComment.
	 */
	//public List<AgreementComment> prepareAgreementComment(Integer agreementRequestId);

	/**
	 * This method is used deleteCommentAttachment
	 * @param vo - object of AgreementVO
	 * @return String response
	 */
	//public String deleteCommentAttachment(AgreementVO vo);

	/**
	 * This method is used downloadCommentAttachment
	 * @param vo - object of AgreementVO
	 * @return String response
	 */
	public ResponseEntity<byte[]> downloadCommentAttachment(AgreementVO vo);

	/**This method is used to addAgreementAttachment.
	 * @param files - files.
	 * @param agreementNote - object of AgreementNote
	 * @return String response.
	 */
	public void addAgreementAttachment(AgreementNote agreementNote, MultipartFile[] files);
}
