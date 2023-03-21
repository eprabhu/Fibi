package com.polus.fibicomp.ip.service;

import java.util.Map;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.ip.pojo.InstituteProposalSpecialReview;
import com.polus.fibicomp.ip.vo.InstProposalVO;
import com.polus.fibicomp.proposal.vo.ProposalVO;

@Service
public interface InstitutionalProposalService {

	/**
	 * This method is used to generate institutional proposal number.
	 * @return institutional proposal number.
	 */
	public String generateInstitutionalProposalNumber();

	/**
	 * This method is used to create institutional proposal.
	 * @param proposalId - Id of the proposal.
	 * @param ipNumber - Institutional proposal number.
	 * @param userName - Username of the logged in user.
	 * @return a boolean value.
	 */
	public boolean createInstitutionalProposal(Integer proposalId, String ipNumber, String userName);
	
	/**
	 * This method is used to load institute proposal by id.
	 * @param proposalId - Id of the institute proposal.
	 * @param personId - person id of the current user.
	 * @return institute proposal object.
	 */
	public String loadInstProposalById(Integer proposalId, String personId);

	/**
	 * This method is used to download institute proposal attachment by attachment id.
	 * @param attachmentId - Id attachments of the institute proposal.
	 * @return a byte object.
	 */
	public ResponseEntity<byte[]> downloadInstProposalAttachment(Integer attachmentId);

	/**
	 * This method is used to saveOrUpdateInstituteProposal.
	 * @param vo - Object of the institute proposal.
	 * @return String Object of the institute proposal.
	 * @throws ApplicationException 
	 */
	public String saveOrUpdateInstituteProposal(InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to sort a institute proposal attachments.
	 * @param vo - Object of ProposalVO class.
	 * @return vo object to sorted institute proposal attachments.
	 */
	public String getInstituteProposalAttachments(Integer proposalId);

	/**
	 * This method is used to create new Ip version of the existing IP for modifying the IP
	 * @param vo
	 * @return new version of IP
	 * @throws ApplicationException
	 */
	public String createNewIPVersion(InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to change the IP status
	 * @param vo
	 * @return updated data
	 * @throws ApplicationException
	 */
	public String changeIPStatus(InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to save the Key persons in IP
	 * @param vo
	 * @return saved data
	 * @throws ApplicationException
	 */
	public String saveOrUpdateIPKeyPerson(InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to delete the key person by key person id
	 * @param keyPersonId
	 * @param proposalId
	 * @return success/failure
	 */
	public String deleteIPKeyPerson(Integer keyPersonId, Integer proposalId);

	/**
	 * This method is used to add the IP person attachment
	 * @param files
	 * @param formDataJson
	 * @return added attachment details
	 * @throws ApplicationException
	 */
	public String addIPPersonAttachment(MultipartFile[] files, String formDataJson) throws ApplicationException;

	/**
	 * This method is used to save the IP special review
	 * @param vo
	 * @return added special review
	 * @throws ApplicationException
	 */
	public String saveOrUpdateIPSpecialReview(InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to save the IP area of research
	 * @param vo
	 * @return added area of research
	 * @throws ApplicationException
	 */
	public String saveOrUpdateIPAreaOfResearch(InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to save the IP budget details
	 * @param vo
	 * @return saved budget details
	 * @throws ApplicationException
	 */
	public String saveOrUpdateIPBudget(InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to delete the IP special review by id and proposal id
	 * @param specialReviewId
	 * @param proposalId
	 * @return success/failure
	 */
	public String deleteIPSpecialReview(Integer specialReviewId, Integer proposalId);

	/**
	 * This method is used to delete the area of research
	 * @param areaOfResearchId
	 * @param proposalId
	 * @return success/failure
	 */
	public String deleteIPAreaOfResearch(Integer areaOfResearchId, Integer proposalId);

	/**
	 * This method is used to delete the IP budget data
	 * @param budgetHeaderId
	 * @param proposalId
	 * @return success/failure
	 */
	public String deleteIPBudgetData(Integer budgetHeaderId, Integer proposalId);

	/**
	 * This method is used to add the IP attachment
	 * @param files
	 * @param formDataJson
	 * @return added attachment
	 * @throws ApplicationException
	 */
	public String addIPAttachment(MultipartFile[] files, String formDataJson) throws ApplicationException;

	/**
	 * This method is used to delete the ip attachment 
	 * @param attachmentId
	 * @param proposalId
	 * @return success/failure
	 */
	public String deleteIPAttachment(Integer attachmentId, Integer proposalId);

	/**
	 * This method is used to update the IP attachment details
	 * @param vo
	 * @return updated details
	 * @throws ApplicationException
	 */
	public String updateIPAttachmentDetails(InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to submit the IP after edit is completed
	 * @param vo
	 * @return updated details
	 * @throws ApplicationException
	 */
	public String submitInstituteProposal(InstProposalVO vo) throws ApplicationException;


	/**
	 * This method is used to delete the IP keyword
	 * @param keywordId
	 * @param proposalId
	 * @return success/failure
	 */
	public String deleteIPKeyword(Integer keywordId, Integer proposalId);

	/**
	 * This method is used to load the proposal budget data by proposalid
	 * @param proposalId
	 * @return IP budget details
	 */
	public String loadIPBudgetsByProposalId(Integer proposalId);

	/**
	 * This method is used to add the more information in IP details
	 * @param vo
	 * @return updated proposal details
	 */
	public String saveIPMoreInformation(InstProposalVO vo);

	/**
	 * This method is used to get the IP action history
	 * @param proposalId
	 * @return action details of selected IP
	 */
	public String getIPActionHistory(Integer proposalId);

	/**
	 * This method is used to get the created version of IP
	 * @param vo
	 * @return proposal id
	 * @throws ApplicationException
	 */
	public Integer getCreatedIPVersion(InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to merge the dev proposal to institute proposal
	 * @param proposalId
	 * @param vo
	 * @return updated development proposal details
	 * @throws ApplicationException
	 */
	public String mergeProposalToIP(Integer proposalId, InstProposalVO vo) throws ApplicationException;

	/**
	 * This method is used to check whether the given institute proposal is able to do the merge
	 * @param proposalNumber
	 * @return true of false
	 */
	public Boolean canMergeDevelopmentProposalToIP(String proposalNumber);

	/**
	 * This method is used to generate the institute proposal on demand
	 * @param vo
	 * @return updated proposal details with ip nummber
	 */
	public String generateInstituteProposal(ProposalVO vo);

	/**
	 * This method is used to check the IP's principal investigator and dev proposal's principal investigator are same or not
	 * @param proposalNumber
	 * @param devProposalId
	 * @return true of false
	 */
	public Boolean checkForPIDifference(String proposalNumber, Integer devProposalId);

	/**
	 * This method is used to get the base proposal title by proposal number
	 * @param baseProposalNumber
	 * @return base proposal title
	 */
	public String getIPTitleForMasterProposal(String baseProposalNumber);

	/**
	 * This method is used to fetch a institute proposal comments.
	 * @param proposalId
	 * @return institute proposal Comment list.
	 */
	public String fetchInstituteProposalComments(Integer proposalId);

	/**
	 * This method is used to save or update institute proposal comments.
	 * @return String object of InstProposalVO
	 */
	public String saveOrUpdateInstituteProposalComment(InstProposalVO vo);

	/**
	 * This method is used to set InsSpecialReviewDetail.
	 * @param persons,nonEmployees and instituteProposalSpecialReview
	 * @return InstituteProposalSpecialReview
	 */
	public InstituteProposalSpecialReview setInsSpecialReviewDetail(Map<String, String> persons,
			Map<String, String> nonEmployees, InstituteProposalSpecialReview instituteProposalSpecialReview);

	/**
	 * @param vo
	 * @return
	 */
	public String showInstituteProposalHistory(InstProposalVO vo);

}
