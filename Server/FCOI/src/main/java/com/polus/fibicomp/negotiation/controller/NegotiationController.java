package com.polus.fibicomp.negotiation.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.negotiation.dto.NegotiationDataBus;
import com.polus.fibicomp.negotiation.service.NegotiationService;
import com.polus.fibicomp.negotiation.vo.NegotiationActivityVO;
import com.polus.fibicomp.negotiation.vo.NegotiationAssociationVO;
import com.polus.fibicomp.negotiation.vo.NegotiationVO;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.OrganizationSearchResult;

@RestController
public class NegotiationController {

	protected static Logger logger =LogManager.getLogger(NegotiationController.class.getName());
	
	@Autowired
	@Qualifier(value = "negotiationService")
	private NegotiationService  negotiationService;
	
	@Autowired
	@Qualifier(value = "commonService")
	private CommonService  commonService;
	
	@Autowired
	@Qualifier(value = "userDocumentAuthorization")
	private UserDocumentAuthorization documentAuthorization;
	
	@GetMapping(value = "/getLocation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getLocation(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for get location");
		return negotiationService.getLocation();
	}
	
	@PostMapping(value = "/getLocationById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getLocationById(@RequestBody NegotiationVO negotiationVO ,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for get location. The input parameters are "+negotiationVO);
		return negotiationService.getLocationById(negotiationVO);
	}
	
	@GetMapping(value = "/getActivityType", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getActivityType(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for get activity type list ");
		return negotiationService.getActivityTypeList();
	}
	
	@PostMapping(value = "/getNegotiationsActivityById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getNegotiationsActivityById(@RequestBody NegotiationVO negotiationVO,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for get activity type list ");
		return negotiationService.getNegotiationsActivityById(negotiationVO);
	}
	
	@PostMapping(value = "/addNegotiationActivity", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addNegotiationActivity(@RequestBody NegotiationVO negotiationsActivityTypeVo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for add negotiation activity"+negotiationsActivityTypeVo);
		return negotiationService.addNegotiationActivity(negotiationsActivityTypeVo);
	}
	
	@GetMapping(value = "/getNegotiationAssociationType", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getNegotiationAssociationType(HttpServletRequest request, HttpServletResponse response){
		logger.info("Requesting for get association type as list");
		return negotiationService.getNegotiationAssociationTypeList();
	}
		
	@PostMapping(value = "/saveNegotiationInfo", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveNegotiation(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("********saveNegotiationInfo API************");
		return negotiationService.saveNegotiationInfo(vo);
	}
	
	@PostMapping(value = "/loadNegotiation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<String> viewNegotiation(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("********viewNegotiation API************");
		 HttpStatus httpStatus = HttpStatus.OK; 
         if(vo.getNegotiationId()  != null &&
        		 !documentAuthorization.isAuthorized(5, vo.getNegotiationId().toString(), vo.getPersonId())) {
                 httpStatus = HttpStatus.FORBIDDEN;        
                 return new ResponseEntity<String>("Not Authorized to view this Negotiation Document", httpStatus);
         }
        String responseData = negotiationService.loadNegotiation(vo);
        return new ResponseEntity<String>(responseData, httpStatus);		
	}
	
	@PostMapping(value = "/getNegotiationLocationHistory", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getNegotiationLocationHistory(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("********getNegotiationLocationHistory API************");
		return negotiationService.getNegotiationLocationHistory(vo);
	}
	
	@PostMapping(value = "/setNegotiationLocation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveNegotiationLocation(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("********setNegotiationLocation API************");
		return negotiationService.setNegotiationLocation(vo);
	}
	
	@PostMapping(value = "/getLastNegotiationLocationDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getLastNegotiationLocationDetails(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("********getLastNegotiationLocationDetails API************");
		return negotiationService.getLastLocationDetails(vo);
	}
	
	
	@PostMapping(value = "/maintainNegotiationAgreement", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String maintainNegotiationAgreement(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("********maintainNegotiationAgreement API************");
		return negotiationService.maintainNegotiationAgreement(vo);
	}
		
	@PostMapping(value = "/maintainNegotiationPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addPerson(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("********addPerson API************");
		return negotiationService.maintainNegotiationPerson(vo);
	}
	
	@PostMapping(value = "/maintainNegotiationActivity")
	public String maintainNegotiationActivity(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for maintainNegotiationActivity");
		return negotiationService.maintainNegotiationActivity(files, formDataJson);
	}
		
	@PostMapping(value = "/addNegotiationsAssociation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addNegotiationsAssociationDetails(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addNegotiationsAssociationDetails"); 
		return negotiationService.addNegotiationsAssociationDetails(vo);
	}
	
	@PostMapping(value = "/addNegotiationsAttachments")
	public String addNegotiationsAttachments(@RequestParam(value = "file", required = false) MultipartFile[] file,
			@RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addNegotiationsAttachments");
		return negotiationService.addNegotiationsAttachment(file, formDataJson);
	}

	@PostMapping(value = "/deleteAssociatedDetail", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAssociatedDetail(@RequestBody NegotiationVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("deleteUnassociatedDetail API");
		return negotiationService.deleteAssociatedDetail(vo);
	}
	
	@GetMapping(value = "/downloadNegotiationAttachment")
	public ResponseEntity<byte[]> downloadNegotiationAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String negotiationsAttachmentId) {
		logger.info("Requesting for downloadNegotiationAttachment");
		logger.info("negotiationsAttachmentId : " + negotiationsAttachmentId);
		Integer attachmentid = Integer.parseInt(negotiationsAttachmentId);
		return negotiationService.downloadNegotiationAttachment(attachmentid);
	}
	
	@PostMapping(value = "/generateNegotiationReport")
	public ResponseEntity<byte[]> generateNegotiationReport(@RequestBody NegotiationDataBus negotiationDataBus,
			HttpServletRequest request, HttpServletResponse response) throws Exception {
		return negotiationService.generateNegotiationReport(response, negotiationDataBus);
	}

	@PostMapping(value = "/maintainNegotiationAssociation",produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String maintainNegotiationAssociation(@RequestBody NegotiationAssociationVO vo, HttpServletRequest request, HttpServletResponse response) {
 		logger.info("Requesting for Negotiation Association "); 
		return negotiationService.maintainNegotiationAssociation(vo);
	}
	
	@PostMapping(value = "/addNegotiationsActivityDashboard")
	public String addNegotiationsActivityDashboard(@RequestParam(value = "file", required = false) MultipartFile[] file,
			@RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addNegotiationsActivityDashboard");
		return negotiationService.addNegotiationsActivityDashboard(file, formDataJson);
	}

	@PostMapping(value = "/deleteActivityAttachments", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteActivityAttachments(@RequestBody NegotiationActivityVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("deleteActivityAttachments API");
		return negotiationService.deleteActivityAttachments(vo);
	}
	
	@PostMapping(value = "/deleteActivity", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteActivity(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("delete Negotiation Activity");
		return negotiationService.deleteActivity(vo);
	}
	
	@PostMapping(value = "/submitNegotiation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String submitNegotiation(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("controller for submit negotiation");
		return negotiationService.submitNegotiation(vo);
	}
	
	@PostMapping(value = "/findSubawardOrganizations")
	public List<OrganizationSearchResult> getNext(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for subAwards");
		return negotiationService.findSubawardOrganisations(vo.getSearchString());
	}

	@PostMapping(value = "/loadNegotiationAttachments", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadNegotiationAttachments(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadNegotiationAttachments");
		return negotiationService.loadNegotiationAttachments(vo);
	}

	@PostMapping(value = "/exportSelectedNegotiationAttachments")
	public void exportSelectedAttachments(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for exportSelectedNegotiationAttachments");
		logger.info("negotiationId " + vo.getNegotiationId());
		negotiationService.exportSelectedNegotiationAttachments(vo, response);
	}

	@PostMapping(value = "/deleteNegotiationAttachment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteNegotiationAttachments(@RequestBody NegotiationVO vo, HttpServletRequest request,HttpServletResponse response) {
		logger.info("Requesting for deleteNegotiationAttachment");
		return negotiationService.deleteNegotiationAttachment(vo);
	}

	@PostMapping(value = "/deleteNegotiationLocation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteNegotiationLocation(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		return negotiationService.deleteNegotiationLocation(vo);
	}

	@PostMapping(value = "/getAttachmentActivityDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAttachmentActivityDetails(@RequestBody NegotiationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getAttachmentActivityDetails");
		logger.info("NegotiationActivityId " + vo.getNegotiationActivityId());
		logger.info("NegotiationLocationId " + vo.getNegotiationLocationId());
		return negotiationService.getAttachmentActivityDetails(vo);
	}

}
