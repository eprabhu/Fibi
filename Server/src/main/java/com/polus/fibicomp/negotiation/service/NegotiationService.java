package com.polus.fibicomp.negotiation.service;

import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.medusa.dto.MedusaDTO;
import com.polus.fibicomp.negotiation.dto.NegotiationDataBus;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.negotiation.vo.NegotiationActivityVO;
import com.polus.fibicomp.negotiation.vo.NegotiationAssociationVO;
import com.polus.fibicomp.negotiation.vo.NegotiationMode;
import com.polus.fibicomp.negotiation.vo.NegotiationVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.vo.OrganizationSearchResult;

@Service
public interface NegotiationService {

	/**
	 * this method is used for get location
	 * @return list of locations
	 */
	public String getLocation();
	
	/**
	 * this method is used for get location by id
	 * @return list of locations
	 */
	public String getLocationById(NegotiationVO negotiationVO);
	
	/**
	 * This method is used for get activity type list
	 */
	public String getActivityTypeList();
	
	/**
	 * This method is used for get activity type list
	 * @param negotiationVo
	 * return activity as an object
	 */
	public String getNegotiationsActivityById(NegotiationVO negotiationVO);
	
	/**
	 * @param negotiationsActivityTypeVo
	 * @return success or fail message 
	 */
	public String addNegotiationActivity(NegotiationVO negotiationsActivityTypeVo);
	
	/**
	 * this method is used for get list of association types
	 * @return list of negotiation association types
	 */
	public String getNegotiationAssociationTypeList();
	
	/**
	 * This method is used to save a Negotiation and its details.
	 * 
	 * @param negotiationVO
	 * @return it returns Success message.
	 */
	
	public String saveNegotiationInfo(NegotiationVO negotiationVO);
	/**
	 * This method is used to view a particular Negotiation by negotiationId.
	 * 
	 * @param negotiationVO
	 * @return it returns the negotiation details.
	 */
	
	public String loadNegotiation(NegotiationVO negotiationVO);
	
	/**
	 * This method is used to get the location history details.
	 * 
	 * @param negotiationVO
	 * @return it returns the negotiation location history list.
	 */
	
	public String getNegotiationLocationHistory(NegotiationVO negotiationVO);
	
	/**
	 * This method is used to save the negotiation location details.
	 * 
	 * @param negotiationVO
	 * @return it returns success message.
	 */
	
	public String setNegotiationLocation(NegotiationVO negotiationVO);
	
	/**
	 * This method is used to get the last negotiation location details.
	 * 
	 * @param negotiationVO
	 * @return it returns last negotiation location details.
	 */
	
	public String getLastLocationDetails(NegotiationVO negotiationVO);

	/**
	 * This method is used to add a new period for the negotiation Agreement value.
	 * 
	 * @param negotiationVO
	 * @return it returns Success.
	 */
	
	public String maintainNegotiationAgreement(NegotiationVO negotiationVO);
	/**
	 * 
	 * @param files
	 * @param formDataJson
	 * @return
	 */
	public String maintainNegotiationActivity(MultipartFile[] files, String formDataJson);
	
	
	
	/**
	 * This method is used to add a new Person.
	 * 
	 * @param negotiationVO
	 * @return it returns Success.
	 */
	
	public String maintainNegotiationPerson(NegotiationVO negotiationVO);
	
	/**
	 * This method is used to get all Persons information.
	 * 
	 * @param negotiationVO
	 * @return it returns list of persons details.
	 */

	public String addNegotiationsAssociationDetails(NegotiationVO negotiationVO);
	
	/**
	 * @param files
	 * @param formDataJson
	 * @return
	 */
	public String addNegotiationsAttachment(MultipartFile[] files, String formDataJSON);
	
	/**
	 * this method is used for add multimedia files as activity attachment
	 * @param files
	 * @param formDataJson
	 * @return
	 */
	public String addNegotiationsActivityDashboard(MultipartFile[] files, String formDataJSON);

	/**
	 * this method is used for delete association details
	 * @param vo
	 * @return
	 */
	public String deleteAssociatedDetail(NegotiationVO vo);

	/**
	 * this method is used for delete associations
	 * @param vo
	 * @return
	 */
	public String deleteActivity(NegotiationVO vo);
	
	/**
	 * 
	 * @param attachmentId
	 * @return
	 */
	public ResponseEntity<byte[]> downloadNegotiationAttachment(Integer attachmentId);

	/**
	 * 
	 * @param response
	 * @param negotiationDataBus
	 * @return
	 */
	ResponseEntity<byte[]> generateNegotiationReport(HttpServletResponse response,
			NegotiationDataBus negotiationDataBus);

	/**
	 * this method is used for maintain negotiations associations
	 * @param vo
	 * @return
	 */
	public String maintainNegotiationAssociation(NegotiationAssociationVO vo);

	/**
	 * this method is used for get medusa details
	 * @param vo
	 * @return
	 */
	public String getMedusaMoreDetails(MedusaDTO vo);

	/**
	 * this method is used for delete associations details
	 * @param vo
	 * @return
	 */
	public String deleteNegotiationAssociatedDetail(NegotiationAssociationVO vo);

	/**
	 * this method is used for delete associations
	 * @param vo
	 * @return
	 */
	public String deleteNegotiationAssociation(NegotiationAssociationVO vo);

	/**
	 * This method is used for delete negotiations attachment
	 * @param vo
	 * @return
	 */
//	public String deleteNegotiationsAttachments(NegotiationActivityVO vo);

	/**
	 * This method is used for delete activity attachment
	 * @param vo
	 * @return
	 */
	public String deleteActivityAttachments(NegotiationActivityVO vo);
	
	public String deleteAttachment(Integer negotiationsActivityId);

	/**
	 * this method is used for submit negotiation .in this action validation check ,
	 * workflow building and notification actions will done 
	 * @param vo
	 * @return
	 */
	public String submitNegotiation(NegotiationVO vo);

	/**
	 * subaward organisation search 
	 * @param searchString
	 * @return
	 */
	public List<OrganizationSearchResult> findSubawardOrganisations(String searchString);

	/**
	 * This method is used to fetch all negotiation attachments
	 * @param vo
	 * @return list of attachments
	 */
	public String loadNegotiationAttachments(NegotiationVO vo);

	/**
	 * This method is used to export selected negotiation attachment
	 * @param vo
	 * @param response
	 */
	public void exportSelectedNegotiationAttachments(NegotiationVO vo, HttpServletResponse response);

	/**
	 * This method is used to delete negotiation attachment
	 * @param vo
	 * @return
	 */
	public String deleteNegotiationAttachment(NegotiationVO vo);

	public NegotiationVO sendNegotiationNotification(NegotiationVO vo, Integer notificationTypeId,Set<NotificationRecipient> dynamicEmailrecipients);

	public String deleteNegotiationLocation(NegotiationVO vo);

	/**
	 * This method is used to load Negotiation User Name
	 * @param negotiation
	 * @return
	 */
	public void loadNegotiationUserFullNames(Negotiations negotiation);

	/**
	 * This method is used to get NegotiationMode
	 * @param negotiation
	 * @return
	 */
	public NegotiationMode getNegotiationMode(Negotiations negotiation);

	/**
	 * @param vo
	 * @return
	 */
	public String getAttachmentActivityDetails(NegotiationVO vo);

}
