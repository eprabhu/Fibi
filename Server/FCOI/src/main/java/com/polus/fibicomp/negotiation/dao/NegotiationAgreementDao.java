package com.polus.fibicomp.negotiation.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.negotiation.pojo.NegotiationCommentAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationLocationStatus;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivity;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivityType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAgreementType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationDetails;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachmentType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsComment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsCommentType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocationType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsPersonnelType;
import com.polus.fibicomp.negotiation.pojo.NegotiationsStatus;
import com.polus.fibicomp.negotiation.pojo.NegotiationsWorkflowStatus;

@Service
public interface NegotiationAgreementDao {

	/**
	 * This method is used for get locations By Id
	 * 
	 * @return it returns location list.
	 */
	public List<NegotiationsLocation> getLocationByNegotiationId(Integer negotiationId);

	/**
	 * this method is used for get negotiations association type as list
	 * 
	 * @return get negotiation association type list
	 */

	public List<NegotiationsAssociationType> getNegotiationAssociationTypes();

	/**
	 * This method is used to get Negotiation Activity List.
	 * 
	 * @param
	 * @return it returns complete list of negotiationsActivity.
	 */

	public List<NegotiationsActivity> getNegotiationsActivityByNegotiationId(Integer negotiationId);

	/**
	 * This method is used to get Negotiation status List.
	 * 
	 * @param
	 * @return it returns complete list of negotiationsStatus.
	 */
	public List<NegotiationsStatus> getAllNegotiationsStatus();

	/**
	 * This method is used to get Negotiation Agreement types List.
	 * 
	 * @param
	 * @return it returns complete list of negotiationsTypes.
	 */
	public List<NegotiationsAgreementType> getNegotiationsAgreementTypes();

	/**
	 * This method is used to get Negotiations Attachment Types.
	 * @param
	 * @return List of NegotiationsAttachmentType.
	 */
	public List<NegotiationsAttachmentType> getNegotiationsAttachmentTypes();

	/**
	 * This method is used to get Negotiation Location List.
	 * @param
	 * @return it returns complete list of negotiationsLocation.
	 */

	public List<NegotiationsLocationType> getNegotiationsLocationTypes();

	/**
	 * This method is used to get Negotiation Personnel List.
	 * 
	 * @param
	 * @return it returns complete list of negotiationsPersonnel.
	 */

	public List<NegotiationsPersonnelType> getNegotiationsPersonnelTypes();

	/**
	 * This method is used to get Negotiation Workflow Status List.
	 * 
	 * @param
	 * @return it returns complete list of negotiationsWorkflowStatus.
	 */

	public List<NegotiationsWorkflowStatus> getNegotiationsWorkflowStatus();

	/**
	 * This method is used to save a Negotiation.
	 * 
	 * @param negotiations
	 * @return Success message.
	 */

	public Negotiations saveNegotiationInfo(Negotiations negotiations);

	/**
	 * This method is used to get Negotiation Activity List.
	 * 
	 * @param
	 * @return it returns complete list of negotiationsActivity.
	 */

	public List<NegotiationsActivityType> getNegotiationsActivityTypes();

	/**
	 * This method is used to save the negotiation location.
	 * 
	 * @param negotiationsLocation
	 * @return it returns success message.
	 */
	public NegotiationsLocation saveOrUpdateNegotiationLocation(NegotiationsLocation negotiationsLocation);

	/**
	 * this method is used for remove association details
	 * 
	 * @param associationDetail
	 * @return
	 */
	public String deleteAssociatedDetail(NegotiationsAssociationDetails associationDetail);

	/**
	 * this method is used for delete activity
	 * 
	 * @param negotiationsActivity
	 * @return
	 */
	public String deleteActivity(NegotiationsActivity negotiationsActivity);

	/**
	 * 
	 * @param attachmentId
	 * @return
	 */
	public NegotiationsAttachment fetchAttachmentById(Integer attachmentId);

	/**
	 * this method is used for get attachment data
	 * @param negotiationsActivityId
	 * @return
	 */
	public List<NegotiationsAttachment> getAttachmentData(Integer negotiationsActivityId);

	/**
	 * this method is used for delete association by id
	 * @param negotiationsAssociationId
	 * @return
	 */
	public String deleteNegotiationAssociation(Integer negotiationsAssociationId);

	/**
	 * this method is used for save association
	 * @param negotiationAssociation
	 * @return
	 */
	public NegotiationsAssociation saveNegotiationAssociation(NegotiationsAssociation negotiationAssociation);

	/**
	 * 
	 * @param negotiationAssocDetailId
	 * @return
	 */
	public String deleteNegotiationAssociationDetails(NegotiationsAssociationDetails negotiationsAssociationDetail);

	/**
	 * this method is used for delete association details
	 * @param negotiationAssociationDetails
	 * @return
	 */
	public String saveNegotiationAssociationDetails(NegotiationsAssociationDetails negotiationAssociationDetails);

	/**
	 * this method is used for save attachments
	 * @param negotiationsAttachment
	 * @return NegotiationsAttachment object
	 */
	public NegotiationsAttachment saveAttachment(NegotiationsAttachment negotiationsAttachment);

	/**
	 * 
	 * @param negotiationsActivity
	 * @return NegotiationsActivity Object
	 */
	public NegotiationsActivity saveOrUpdateActivity(NegotiationsActivity negotiationsActivity);

	/**
	 * This method is used to fetch negotiation with negotiationId
	 * @param negotiationId
	 * @return
	 */
	public Negotiations fetchNegotiationById(Integer negotiationId);

	/**
	 * this method is used to fetchNegotiationStatusByStatusCode
	 * @param negotiationStatusCodeReturned
	 * @return
	 */
	public NegotiationsStatus fetchNegotiationStatusByStatusCode(String negotiationStatusCode);

	/**
	 * This method is used for get negotiations status descriptions with respect to status code 
	 * @param satusCode
	 * @return
	 */
	public NegotiationsStatus fetchStatusByStatusCode(String satusCode);

	/**
	 * this method is used for fetch Negotiations Comment Type ById
	 * @param commentTypeCode
	 * @return object of NegotiationsCommentType
	 */
	public NegotiationsCommentType fetchNegotiationsCommentTypeById(String commentTypeCode);

	/**
	 * this method is used for save Negotiations Comment 
	 * @param negotiationsComment - object of NegotiationsComment
	 * @return object of NegotiationsComment.
	 */
	public NegotiationsComment saveOrUpdateNegotiationsComment(NegotiationsComment negotiationsComment);

	/**
	 * this method is used fetchNegotiationLocationBasedOnParams 
	 * @param negotiationId - negotiationId
	 * @param locationTypeCode - locationTypeCode
	 * @return object of NegotiationsComment.
	 */
	public NegotiationsLocation fetchNegotiationLocationBasedOnParams(Integer negotiationId, String locationTypeCode, String personId);

	/**
	 * this method is used fetchNegotiationLocationBasedOnParams 
	 * @param negotiationId - negotiationId
	 * @param personId - personId
	 * @param locationTypeCode - locationTypeCode
	 * @return object of NegotiationsComment.
	 */
	public List<NegotiationsLocation> fetchNegotiationLocationBasedOnNegotiationId(Integer negotiationId, String personId,  List<String> locationStatuses);

	/**
	 * this method is used fetchNegotiationLocationBasedOnParams 
	 * @param negotiationId - negotiationId
	 * @param personId - personId
	 * @param locationTypeCode - locationTypeCode
	 * @return object of NegotiationsComment.
	 */
	public List<NegotiationsComment> fetchNegotiationCommentBasedOnParams(Integer negotiationLocationId);

	/**
	 * this method is used for  get Negotiation Location Status By Id
	 * @param locationStatusCode
	 * @return object of NegotiationLocationStatus
	 */
	public NegotiationLocationStatus getNegotiationLocationStatusById(String locationStatusCode);

	/**
	 * this method is used for fetch Negotiations Comment Type negotiationId
	 * @param negotiationId
	 * @return List of NegotiationsComment
	 */
	public List<NegotiationsComment> fetchNegotiationCommentByNegotiationId(Integer negotiationId);

	/**
	 * this method is used for get Negotiations Comment By Id
	 * @param negotiationId
	 * @return Object of NegotiationsComment
	 */
	public NegotiationsComment getNegotiationsCommentById(Integer negotiationCommentId);

	/**
	 * this method is used for delete Negotiations Comment
	 * @param negotiationComment
	 */
	public void deleteNegotiationsComment(NegotiationsComment negotiationComment);

	/**
	 * this method is used for fetch Negotiations Comment Attachment by Id
	 * @param attachmentId
	 * @return Object of NegotiationCommentAttachment
	 */
	public NegotiationCommentAttachment fetchNegotiationCommentAttachmentById(Integer attachmentId);

	/**
	 * this method is used for delete Negotiation Comment Attachment
	 * @param negotiationCommentAttachment - Object ofNegotiationCommentAttachment
	 */
	public void deleteNegotiationCommentAttachment(NegotiationCommentAttachment negotiationCommentAttachment);

	/**
	 * This method is used to fetch Location Comment Count
	 * @param negotiationLocationId
	 * @return comment Count
	 */
	public Long fetchLocationCommentCount(Integer negotiationLocationId);

	/**
	 * This method is used to saveOrUpdateNegotiationsAttachment.
	 * @param negotiationsAttachment
	 */
	public void saveOrUpdateNegotiationsAttachment(NegotiationsAttachment negotiationsAttachment);

	/**
	 * this method is used deleteNegotiationAttachment.
	 * @param negotiationsAttachment.
	 * @return
	 */
	public void deleteNegotiationAttachment(NegotiationsAttachment negotiationsAttachment);

	/**
	 * this method is used for fetch Negotiation Association Details
	 * @param negotiationsAssociationId
	 * @return
	 */
	public List<NegotiationsAssociationDetails> fetchNegotiationAssociationDetailsBasedOnNegotiationAssociationId(Integer negotiationsAssociationId);

	/**
	 * This method is used for getNegotiationLocationStatuse
	 * 
	 * @return it returns location list.
	 */
	public List<NegotiationLocationStatus> getNegotiationLocationStatus();

	/**
	 * This method is used to get Negotiation Personnel List.
	 * 
	 * @param
	 * @return it returns complete list of negotiationsPersonnel.
	 */

	public NegotiationsPersonnelType getNegotiationsPersonnelTypesById(String personnelTypeCode);

	/**
	 * This method is used to deleteNegotiationLocation
	 * @param negotiationLocation
	 * @return String value
	 */
	public String deleteNegotiationLocation(NegotiationsLocation negotiationLocation);

	/**
	 * This method is used to getNegotiationLocationById.
	 * @param negotiationLocationId
	 * @return
	 */
	public NegotiationsLocation getNegotiationLocationById(Integer negotiationLocationId);

	/**
	 * This method is used to load all negotiation attachments
	 * @param negotiationId
	 */
	public List<NegotiationsAttachment> loadNegotiationAttachments(Integer negotiationId);

	/**
	 * This method is used to fetch all negotiation attachments by attachment ids
	 * @param attachmentIds
	 * @return List of NegotiationsAttachment
	 */
	public List<NegotiationsAttachment> fetchNegotiationAttachmentBasedOnAttachmentIds(List<Integer> attachmentIds);

	/**
	 * This method is used to fetch Negotiations Attachment Based On NegotiationId
	 * @param negotiationId
	 * @return List of NegotiationsAttachment
	 */
	public List<NegotiationsAttachment> fetchNegotiationsAttachmentBasedOnNegotiationId(Integer negotiationId);

	/**
	 * This method is used to deleteAssociations
	 * @param negotiationsAssociationId
	 * @return String value
	 */
	public String deleteAssociations(Integer negotiationsAssociationId);

	/**
	 * This method is getUserHaveReview
	 * @param negotiationId
	 * @param personId
	 * @return String value
	 */
	public Boolean getUserHaveReview(Integer negotiationId, String personId);

	/**
	 * This method is used to fetch getNegotiationsActivity By LocationId
	 * @param negotiationLocationId
	 * @return List of NegotiationsActivity
	 */
	public List<NegotiationsActivity> getNegotiationsActivityByLocationId(Integer negotiationLocationId);

	/**
	 * @param negotiationsActivityId
	 * @return
	 */
	public String deleteActivityAttachments(Integer negotiationsActivityId);

	/**
	 * This method is used to get negotiation activity.
	 * @param negotiationsActivityId
	 * @return
	 */
	public NegotiationsActivity getNegotiationsActivityByActivityId(Integer negotiationsActivityId);

}
