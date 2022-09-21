package com.polus.fibicomp.award.version.service;

import javax.servlet.http.HttpServletRequest;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardHistoryLog;
import com.polus.fibicomp.award.vo.AwardLinkInstituteProposalVO;
import com.polus.fibicomp.award.vo.AwardVO;

@Transactional
@Service(value = "awardVersionService")
public interface AwardVersionService {

	/**
	 * This method is used to submit Award.
	 * @param awardVO - object of awardVO.
	 * @return a String of details of Award.
	 */
	public String copyAward(AwardVO awardVO);

	/**
	 * This method is used to createAwardVariationRequest.
	 * @param files        - attached files.
	 * @param request 
	 * @param formDataJSON - form data for the and comment attachments.
	 * @return object to complete Award review.
	 */
	public String createAwardVariationRequest(MultipartFile[] files, String formDataJson, HttpServletRequest request);

	/**
	 * This method is used to createAwardFromProposal.
	 * @param vo -object of AwardVO.
	 */
	public void createAwardFromProposal(AwardLinkInstituteProposalVO vo);

	/**
	 * This method is used to add award editable fields.
	 * @param vo - AwardVO.
	 */
	public void addAwardEditableFields(AwardVO vo);

	/**
	 * This method is used to create award variation request for WAF
	 * @param vo
	 * @return details of award variation request
	 */
	public String createAwardVariationRequestForWaf(AwardVO vo);

	/**
	 * @param award
	 * @param awardHistoryLog
	 */
	public void addToAwardHistoryLog(Award award, AwardHistoryLog awardHistoryLog);
}
