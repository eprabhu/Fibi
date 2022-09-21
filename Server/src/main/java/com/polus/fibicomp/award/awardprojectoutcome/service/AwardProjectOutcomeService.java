package com.polus.fibicomp.award.awardprojectoutcome.service;

import java.math.BigDecimal;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.award.awardprojectoutcome.dto.AwardOutcomeDTO;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "awardProjectOutcomeService")
public interface AwardProjectOutcomeService {

	/**
	 * This method is used to submit Award.
	 * @param vo - vo
	 * @return A list of details of Publication.
	 */
	public String findPublications(CommonVO vo);

	/**
	 * This method is used to save Award Publication.
	 * @param vo - awardVO object
	 * @return A list of details saved Publication.
	 */
	public String saveAwardPublication(AwardVO vo);

	/**
	 * This method is used to delete Award Publication.
	 * @param vo - awardVO object 
	 * @return A String response.
	 */
	public String deleteAwardPublication(AwardVO vo);

	/**
	 * This method is used to load All Award Project Outcomes.
	 * @param vo - awardVO object
	 * @return A String of details saved AwardVo.
	 */
	public String loadAllAwardProjectOutcomes(AwardVO vo);

	/**
	 * This method is used to save Award Association.
	 * @param vo - awardVO object
	 * @return A list of details saved Association.
	 */
	public String saveAwardAssociation(AwardVO vo);

	/**
	 * This method is used to delete Award Association.
	 * @param vo - awardVO object
	 * @return A String response.
	 */
	public String deleteAwardAssociation(AwardVO vo);

	/**
	 * This method is used to add attachments for a Award Acheivement.
	 * @param files - attached files.
	 * @param formDataJSON - form data for the attachment.
	 * @return A String of details of Award Acheivement data with list of attachments and comments.
	 */
	public String addAwardAcheivements(MultipartFile[] files, String formDataJson);

	/**
	 * This method is used to delete Award Acheivements.
	 * @param vo - awardVO object
	 * @return A String response.
	 */
	public String deleteAwardAcheivements(AwardVO vo);

	public ResponseEntity<byte[]> downloadAwardAcheivementsAttachment(Integer awardAcheivementId);

	public String addAwardAcheivementsForWaf(AwardVO vo);

	/**
	 * This method is used to get Module Details Based On Id.
	 * @param vo - Object of AwardOutcomeDTO.
	 * @return A String of details of module.
	 */
	public String getModuleDetails(AwardOutcomeDTO vo);

	/**
	 * This method is used to get total Award Granted Amount Based On Id.
	 * @param awardId.
	 * @return total award amount.
	 */
	public BigDecimal totalAwardGrantedAmount(Integer awardId);

}
