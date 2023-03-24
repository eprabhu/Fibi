package com.polus.fibicomp.award.awardprojectoutcome.dao;

import java.sql.Timestamp;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAcheivements;
import com.polus.fibicomp.award.pojo.AwardAssociation;
import com.polus.fibicomp.award.pojo.AwardAssociationDetail;
import com.polus.fibicomp.award.pojo.AwardAssociationType;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPublications;
import com.polus.fibicomp.award.pojo.Publication;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service
public interface AwardProjectOutcomeDao {

	/**
	 * This method is used to fetch filtered Publication based on input string.
	 * @param vo - vo.
	 * @return a list of publication.
	 */
	public List<Publication> findPublications(CommonVO vo);

	/**
	 * This method is used to fetch filtered Publication based on input string.
	 * @param searchString - input string.
	 * @return a list of publication.
	 */
	public AwardPublications saveOrUpdateAwardPublications(AwardPublications awardPublication);

	/**
	 * This method is used to  Award Publications based on awardPublicationId.
	 * @param awardPublicationId - awardPublicationId.
	 * @return an object of AwardPublications.
	 */
	public AwardPublications getAwardPublicationsBasedOnId(Integer awardPublicationId);

	/**
	 * This method is used to  deleteAwardPublication.
	 * @param awardPublication - awardPublication object.
	 * @return an object of AwardPublications.
	 */
	public AwardPublications deleteAwardPublication(AwardPublications awardPublication);

	/**
	 * This method is used to  findAllAwardPublications.
	 * @param awardId - awardId.
	 * @return an List of AwardPublications.
	 */
	public List<AwardPublications> fetchAllAwardPublications(Integer awardId);

	/**
	 * This method is used to fetch filtered Publication based on input string.
	 * @param searchString - input string.
	 * @return a list of publication.
	 */
	public AwardAssociation saveOrUpdateAwardAssociation(AwardAssociation awardAssociation);

	/**
	 * This method is used to  Award Publications based on awardAssociationId.
	 * @param awardAssociationId - awardAssociationId.
	 * @return an object of AwardAssociation.
	 */
	public AwardAssociation getAwardAssociationBasedOnId(Integer awardAssociationId);

	/**
	 * This method is used to  deleteAwardPublication.
	 * @param awardAssociation - awardAssociation object.
	 * @return an object of AwardAssociation.
	 */
	public AwardAssociation deleteAwardAssociation(AwardAssociation awardAssociation);

	/**
	 * This method is used to  findAllAwardAssociation.
	 * @param awardId - awardId .
	 * @return an List of AwardAssociation.
	 */
	public List<AwardAssociation> fetchAllAwardAssociation(Integer awardId);

	/**
	 * This method is used to fetch filtered Publication based on input string.
	 * @param awardAcheivement - object of awardAcheivement
	 * @return a object of AwardAssociation.
	 */
	public AwardAcheivements saveOrUpdateAwardAcheivements(AwardAcheivements awardAcheivement);

	/**
	 * This method is used to  Award Acheivement based on awardAcheivementAttachId.
	 * @param awardAcheivementAttachId - awardAcheivementAttachId.
	 * @return an object of Award Acheivement.
	 */
	public AwardAcheivements getAwardAcheivementsBasedOnId(Integer awardAcheivementAttachId);

	/**
	 * This method is used to  delete AwardAcheivements.
	 * @param awardAcheivement - awardAcheivement object.
	 * @return an object of awardAcheivement.
	 */
	public AwardAcheivements deleteAwardAcheivement(AwardAcheivements awardAcheivement);

	/**
	 * This method is used to  fetchAllAwardAcheivements.
	 * @param awardId - awardId .
	 * @return an List of AwardAcheivements.
	 */
	public List<AwardAcheivements> fetchAllAwardAcheivements(Integer awardId);

	/**
	 * This method is used to  fetchAllAssociationTypes.
	 * @return an List of AwardAssociationType.
	 */
	public List<AwardAssociationType> fetchAllAssociationTypes();

	/**
	 * This method is used to fetch AwardAssociationDetail by awardAssociationId.
	 * @param awardAssociationId - Id of the awardAssociation.
	 * @return corresponding AwardAssociationDetail object.
	 */
	public AwardAssociationDetail fetchAwardAssociationDetailByAwardAssociationId(Integer awardAssociationId);

	/**
	 * This method is used for save award association details
	 * @param awardAssociationDetail - object of AwardAssociationDetail
	 * @return AwardAssociationDetail object
	 */
	public AwardAssociationDetail saveAwardAssociationDetail(AwardAssociationDetail awardAssociationDetail);

	/**
	 * This method is used for getAwardIdBasedOnParams
	 * @param personIds - List of personIds
	 * @param rolodexIds - List of rolodexIds
	 * @return List of Award Id's
	 */
	public List<Integer> getAwardIdBasedOnParams(List<String> personIds, List<Integer> rolodexIds);

	/**
	 * This method is used for getProposalIdBasedOnParams
	 * @param personIds - List of personIds
	 * @param rolodexIds - List of rolodexIds
	 * @return List of Proposal Id's
	 */
	public List<Integer> getProposalIdBasedOnParams(List<String> personIds, List<Integer> rolodexIds);

	/**
	 * This method is used for getAwardsBasedOnParams
	 * @param awardIds - List of awardIds
	 * @param searchString - String value
	 * @param awardExpirationTimestamp
	 * @return List of Proposal Id's
	 */
	public List<Award> getAwardsBasedOnParams(List<Integer> awardIds, String searchString, String awardNumber, Timestamp awardExpirationTimestamp);

	/**
	 * This method is used for getAwardsBasedOnParams
	 * @param awardIds - List of awardIds
	 * @param searchString - String value
	 * @return List of Proposal Id's
	 */
	public List<Proposal> getProposalsBasedOnParams(List<Integer> proposalIds, List<Integer> proposalStatuses, String searchString);

	/**
	 * This method is used for getAwardPiDetails
	 * @param awardId 
	 * @return Object of AwardPerson
	 */
	public AwardPerson getAwardPiDetails(Integer awardId);

	/**
	 * This method is used for getProposalPiDetails
	 * @param proposalId 
	 * @return Object of ProposalPerson
	 */
	public ProposalPerson getProposalPiDetails(Integer proposalId);

	/**
	 * This method is check if file data already found
	 * @param fileId
	 * @return
	 */
	Boolean getIsFileDataIdFound(String fileId);

	/**
	 * This method is used to get publication types
	 * @return list of publication types
	 */
	public List<String> getPublicationTypes();

}
