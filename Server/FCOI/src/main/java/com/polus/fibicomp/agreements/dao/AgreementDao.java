package com.polus.fibicomp.agreements.dao;

import java.util.HashMap;
import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.agreements.pojo.AgreementActionLog;
import com.polus.fibicomp.agreements.pojo.AgreementActionType;
import com.polus.fibicomp.agreements.pojo.AgreementAssociationDetail;
import com.polus.fibicomp.agreements.pojo.AgreementAssociationLink;
import com.polus.fibicomp.agreements.pojo.AgreementAttachment;
import com.polus.fibicomp.agreements.pojo.AgreementAttachmentFile;
import com.polus.fibicomp.agreements.pojo.AgreementAttachmentStatus;
import com.polus.fibicomp.agreements.pojo.AgreementAttachmentType;
import com.polus.fibicomp.agreements.pojo.AgreementCategory;
import com.polus.fibicomp.agreements.pojo.AgreementClauses;
import com.polus.fibicomp.agreements.pojo.AgreementClausesGroupMapping;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementNote;
import com.polus.fibicomp.agreements.pojo.AgreementNoteAttachment;
import com.polus.fibicomp.agreements.pojo.AgreementPeople;
import com.polus.fibicomp.agreements.pojo.AgreementPeopleType;
import com.polus.fibicomp.agreements.pojo.AgreementPlaceHolder;
import com.polus.fibicomp.agreements.pojo.AgreementReviewType;
import com.polus.fibicomp.agreements.pojo.AgreementSponsor;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorContact;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorContactType;
import com.polus.fibicomp.agreements.pojo.AgreementSponsorType;
import com.polus.fibicomp.agreements.pojo.AgreementStatus;
import com.polus.fibicomp.agreements.pojo.AgreementType;
import com.polus.fibicomp.agreements.pojo.AgreementTypeHistory;
import com.polus.fibicomp.agreements.pojo.AgreementTypeTemplate;
import com.polus.fibicomp.agreements.pojo.AgreementWorkflowStatus;
import com.polus.fibicomp.agreements.pojo.Clauses;
import com.polus.fibicomp.agreements.pojo.ClausesBank;
import com.polus.fibicomp.agreements.pojo.ClausesGroup;
import com.polus.fibicomp.agreements.pojo.SponsorRole;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociation;

@Service
public interface AgreementDao  {

	/**
	 * This method is used to save Agreement details.
	 * 
	 * @param agreement
	 * @return it returns the agreement details.
	 */
	public AgreementHeader saveOrUpdateAgreement(AgreementHeader agreement);

	/**
	 * This method is used to fetch Agreement Type data.
	 * 
	 * @param
	 * @return it returns the Agreement Types.
	 */
	public List<AgreementType> fetchAgreementTypes();

	/**
	 * This method is used to fetch Agreement details.
	 * 
	 * @param agreementRequestId
	 * @return it returns the Agreement details.
	 */
	public AgreementHeader getAgreementById(Integer agreementRequestId);

	/**
	 * This method is used to save Agreement Notes.
	 * 
	 * @param agreementNote
	 * @return it returns the agreement Note details.
	 */
	public AgreementNote saveOrUpdateAgreementNotes(AgreementNote agreementNote);

	/**
	 * This method is used to delete Agreement Notes.
	 * 
	 * @param agreementNote
	 * @return it returns success.
	 */
	public String deleteAgreementNote(AgreementNote agreementNote);

	/**
	 * This method is used to fetch Agreement Action Type data.
	 * 
	 * @param
	 * @return it returns the Agreement Action Types.
	 */
	public List<AgreementActionType> fetchAgreementActionTypes();

	/**
	 * This method is used to delete Agreement.
	 * 
	 * @param agreement
	 * @return it returns success.
	 */
	public String deleteAgreement(AgreementHeader agreement);

	/**
	 * This method is used to fetch Agreement Status.
	 * 
	 * @param
	 * @return it returns the Agreement Status data.
	 */
	public List<AgreementStatus> fetchAgreementStatusTypes();

	/**
	 * This method is used to fetch Agreement Sponsor Contact Types.
	 * 
	 * @param
	 * @return it returns the Agreement Sponsor Contact Types.
	 */
	public List<AgreementSponsorContactType> fetchAgreementSponsorContactTypes();

	/**
	 * This method is used to fetch Agreement Attachment Types .
	 * 
	 * @param
	 * @return it returns the Agreement Attachment Types.
	 */
	public List<AgreementAttachmentType> fetchAgreementAttachmentTypes();

	/**
	 * This method is used to save Agreement Sponsor.
	 * 
	 * @param agreementSponsor
	 * @return it returns the agreement sponsor details.
	 */
	public AgreementSponsor saveOrUpdateAgreementSponsor(AgreementSponsor agreementSponsor);

	/**
	 * This method is used to delete Agreement Sponsor Contact.
	 * 
	 * @param agreementSponsorContact
	 * @return it returns success.
	 */
	public String deleteAgreementSponsorContact(AgreementSponsorContact sponsorContact);

	/**
	 * This method is used to save Agreement sponsor Contact.
	 * 
	 * @param sponsorContact
	 * @return it returns the sponsor Contact details.
	 */
	public AgreementSponsorContact saveOrUpdateAgreementSponsorContact(AgreementSponsorContact sponsorContact);

	/**
	 * This method is used to delete Agreement Sponsor.
	 * 
	 * @param agreementSponsor
	 * @return it returns success.
	 */
	public String deleteAgreementSponsor(AgreementSponsor agreementSponsor);

	/**
	 * This method is used to save Agreement Action Log.
	 * 
	 * @param agreementActionLog
	 * @return it returns the agreementActionLog.
	 */
	public AgreementActionLog createAgreementActionLog(AgreementActionLog agreementActionLog);

	/**
	 * This method is used to save AgreementAttachmentFile object.
	 * 
	 * @param fileData - AgreementAttachmentFile object.
	 * @return AgreementAttachmentFile.
	 */
	public AgreementAttachmentFile saveFileData(AgreementAttachmentFile fileData);

	/**
	 * This method is used to fetch AgreementAttachmentFile object.
	 * 
	 * @param fileData - AgreementAttachmentFile object.
	 * @return AgreementAttachmentFile.
	 */
	public AgreementAttachmentFile getFileDataById(String agreementAttachmentFileId);

	/**
	 * This method is used to delete AgreementAttachmentFile object.
	 * 
	 * @param fileData - AgreementAttachmentFile object.
	 */
	public void deleteFileData(AgreementAttachmentFile fileData);

	/**
	 * This method is used to save Agreement Attachment.
	 * 
	 * @param attachment
	 * @return it returns the Agreement Attachment details.
	 */
	public AgreementAttachment saveOrUpdateAgreementAttachment(AgreementAttachment attachment);

	/**
	 * This method is used to delete Agreement Attachment.
	 * 
	 * @param AgreementAttachment
	 * @return it returns success.
	 */
	public String deleteAgreementAttachments(AgreementAttachment agreementAttachment);

	/**
	 * This method is used to fetch agreement Attachment by id.
	 * 
	 * @param agreementAttachmentId
	 * @return it returns agreement Attachment.
	 */
	public AgreementAttachment fetchAttachmentById(Integer agreementAttachmentId);

	/**
	 * This method is used to delete file.
	 * 
	 * @param agreementAttachmentFileId
	 * @return .
	 */
	public void deleteFileById(String agreementAttachmentFileId);

	/**
	 * This method is used to delete Agreement File data.
	 * 
	 * @param agreementAttachmentFileId
	 * @return it returns success.
	 */
	public String deleteAgreementFileData(String agreementAttachmentFileId);

	/**
	 * This method is used to fetch agreement attachments based on agreement id
	 * @param agreementId - agreementId
	 * @return agreement attachments.
	 */
	public List<AgreementAttachment> fetchAgreementAttachmentsBasedOnAgreementId(Integer agreementId, Boolean isGenerated);

	/**
	 * This method is used to fetch Agreement action type by id.
	 * @param agreementActionTypeId
	 * @return it returns agreementActionType.
	 */
	public AgreementActionType fetchAgreementActionTypeById(String agreementActionTypeId);

	/**
	 * This method is used to fetch agreement status based on status code.
	 * @param statusCode - statusCode.
	 * @return An AgreementStatus object.
	 */
	public AgreementStatus fetchAgreementStatusByStatusCode(String statusCode);

	/**
	 * This method is used to fetch agreement category by category code.
	 * @param agreementCategoryCode - agreementCategoryCode
	 * @return it returns agreementCategory.
	 */
	public AgreementCategory fetchAgreementCategoryByCategoryCode(String agreementCategoryCode);

	/**
	 * This method is used to fetch Agreement categories.
	 * @return it returns the Agreement category list.
	 */
	public List<AgreementCategory> fetchAllAgreementCategory();

	/**
	 * This method is used to fetch AgreementAttachments Based On AgreementId And DocumentId.
	 * @return it returns the AgreementAttachment.
	 */
	public List<AgreementAttachment> fetchAgreementAttachmentsBasedOnAgreementIdAndDocumentId(Integer agreementId, Integer documentId);

	/**
	 * This method is used to get Negotiation Id Based On AgreementId.
	 * @param agreementRequestId - agreementRequestId
	 * @return List of negotiationsAssociation.
	 */
	public List<NegotiationsAssociation> getNegotiationIdBasedOnAgreementId(Integer agreementRequestId);

	/**
	 * This method is used to get get Agreement Sponsors.
	 * @param agreementRequestId - agreementRequestId
	 * @return List of agreementSponsor.
	 */
	public List<AgreementSponsor> getAgreementSponsors(Integer agreementRequestId);

	/**
	 * This method is used to fetch Agreement Sponsor Based On Id.
	 * @param agreementSponsorId - agreementSponsorId
	 * @return Object of agreementSponsor.
	 */
	public AgreementSponsor fetchAgreementSponsorBasedOnId(Integer agreementSponsorId);

	/**
	 * This method is used to fetch Agreement Type Template.
	 * @return List of agreementTypeTemplate.
	 */
	public List<AgreementTypeTemplate> fetchAgreementTypeTemplate();

	/**
	 * This method is used to saveOrUpdateAgreementTypeTemplate.
	 * @param agreementTypeTemplate - object of agreementTypeTemplate
	 * @return Object of AgreementTypeTemplate.
	 */
	public AgreementTypeTemplate saveOrUpdateAgreementTypeTemplate(AgreementTypeTemplate agreementTypeTemplate);

	/**
	 * This method is used to fetch AgreementAttachments Based on AgreementType.
	 * @param agreementTypeCode - agreementTypeCode
	 * @return List of AgreementTypeTemplate.
	 */
	public List<AgreementTypeTemplate> fetchAgreementAttachmentsBasedOnAgreementType(String agreementTypeCode);

	/**
	 * This method is used to saveOrUpdateClausesGroup.
	 * @param clausesGroup - object of ClausesGroup
	 * @return Object of ClausesGroup.
	 */
	public ClausesGroup saveOrUpdateClausesGroup(ClausesGroup clausesGroup);

	/**
	 * This method is used to saveOrUpdateAgreementClausesGroup.
	 * @param agreementClausesGroupMapping - agreementClausesGroupMapping
	 * @return Object of AgreementClausesGroupMapping.
	 */
	public AgreementClausesGroupMapping saveOrUpdateAgreementClausesGroup(AgreementClausesGroupMapping agreementClausesGroupMapping);

	/**
	 * This method is used to get All Agreement PlaceHolders.
	 * @return List of AgreementPlaceHolder.
	 */
	public List<AgreementPlaceHolder> getAllAgreementPlaceHolders();

	/**
	 * This method is used to get Clauses Details.
	 * @return List of ClausesGroup.
	 */
	public List<ClausesGroup> getClausesDetails();

	/**
	 * This method is used to fetch All Agreement Type Based On ClausesGroup.
	 * @param clauseGroupCode - clauseGroupCode
	 * @return List of AgreementClausesGroupMapping.
	 */
	public List<AgreementClausesGroupMapping> fetchAllAgreementTypeBasedOnClausesGroup(Integer clauseGroupCode);

	/**
	 * This method is used to get Max Version Number.
	 * @param agreementTypeCode - agreementTypeCode
	 * @return Max version Number.
	 */
	public Integer getMaxVersionNumber(String agreementTypeCode);

	/**
	 * This method is used to get Clauses Details By AgreementTypeCode.
	 * @param agreementRequestId - agreementRequestId
	 * @return List of AgreementClausesGroupMapping.
	 */
	public List<AgreementClausesGroupMapping> getClausesDetailsByAgreementTypeCode(String agreementTypeCode);

	/**
	 * This method is used to fetch Clauses By Id.
	 * @param clausesCode - clausesCode
	 * @return Object of Clauses.
	 */
	public Clauses fetchClausesById(Integer clausesCode);

	/**
	 * This method is used to deleteClauses.
	 * @param clauses - Object of clauses
	 * @return it returns List of negotiationsAssociation.
	 */
	public void deleteClauses(Clauses clauses);

	/**
	 * This method is used to fetchAgreementGroupMapping.
	 * @param agreementTypeCode - agreementTypeCode
	 * @param clausesGroupCode - clausesGroupCode
	 * @return Object of agreementClausesGroupMapping.
	 */
	public AgreementClausesGroupMapping fetchAgreementGroupMapping(String agreementTypeCode, String clausesGroupCode);

	/**
	 * This method is used to deleteAgreementGroupMapping.
	 * @param agreementClausesGroup - object of agreementClausesGroup
	 */
	public void deleteAgreementGroupMapping(AgreementClausesGroupMapping agreementClausesGroup);

	/**
	 * This method is used to fetch Agreement Type Template By Id.
	 * @param templateId - templateId
	 * @return Object of AgreementTypeTemplate.
	 */
	public AgreementTypeTemplate fetchAgreementTypeTemplateById(Integer templateId);

	/**
	 * This method is used to delete Agreement Type Template
	 * @param templateId - templateId
	 */
	public void deleteAgreementTypeTemplate(AgreementTypeTemplate templateId);

	/**
	 * This method is used to fetch agreement action logs based on agreement id.
	 * @param agreementCategoryCode - agreementCategoryCode
	 * @return agreement action log list.
	 */
	public List<AgreementActionLog> fetchAgreementActionLogsBasedOnAgreementId(Integer agreementRequestId);

	/**
	 * This method is used to fetch agreement template based on agreement type code and final flag.
	 * @param agreementTypeCode - agreementTypeCode
	 * @return agreement action log list.
	 */
	public byte[] fetchAgreementTemplateBasedOnParams(String agreementTypeCode);

	/**
	 * This method is used to fetch agreement view based on agreement id.
	 * @param agreementRequestId - agreementRequestId
	 * @return agreement view list.
	 */
	public HashMap<String, Object> fetchAgreementViewBasedOnAgreementId(Integer agreementRequestId);

	/**
	 * This method is used to fetch agreement clauses based on agreement id.
	 * @param agreementRequestId - agreementRequestId
	 * @return agreement clauses.
	 */
	public List<AgreementClauses> fetchAgreementClausesBasedOnAgreementId(Integer agreementRequestId);

	/**
	 * This method is used to fetch agreement questionnaire based on agreement id.
	 * @param agreementRequestId - agreementRequestId
	 * @return agreement questionnaires.
	 */
	public List<HashMap<String, Object>> fetchAgreementQuestionnaireBasedOnAgreementId(Integer agreementRequestId);

	/**
	 * This method is used to fetch agreement attachment type based on attachment type code.
	 * @param agreementAttachmentTypeCode - agreementAttachmentTypeCode
	 * @return agreement attachment type.
	 */
	public AgreementAttachmentType fetchAgreementAttachmentTypeBasedOnAttachmentTypeCode(String agreementAttachmentTypeCode);

	/**
	 * This method is used to fetch max version of agreement attachment on agreement id and attachment type code.
	 * agreementRequestId - agreementRequestId
	 * @param agreementAttachmentTypeCode - agreementAttachmentTypeCode
	 * @return agreement attachment.
	 */
	public Integer fetchMaxVersionAgreementAttachmentBasedOnParams(Integer agreementRequestId, String agreementAttachmentTypeCode);

	/**
	 * This method is used to fetch all agreement attachment on agreement id and attachment type code.
	 * agreementRequestId - agreementRequestId
	 * @param agreementAttachmentTypeCode - agreementAttachmentTypeCode
	 * @return agreement attachment list.
	 */
	public List<AgreementAttachment> fetchAllAgreementAttachmentsBasedOnParams(Integer agreementRequestId, String agreementAttachmentTypeCode);

	/**
	 * This method is used to fetch Agreement Type Template Based On Params.
	 * @param agreementTypeCode - agreementTypeCode
	 * @return AgreementTypeTemplate - Object of AgreementTypeTemplate.
	 */
	public AgreementTypeTemplate fetchAgreementTypeTemplateBasedOnParams(String agreementTypeCode);

	/**
	 * This method is used to findClauses.
	 * @param searchString - searchString
	 * @return List of Clauses.
	 */
	public List<ClausesBank> findClauses(String searchString);

	/**
	 * This method is used to saveOrUpdateAgreementClauses.
	 * @param agreementClauses - agreementClauses
	 * @return Object of agreementClauses.
	 */
	public AgreementClauses saveOrUpdateAgreementClauses(AgreementClauses agreementClauses);

	/**
	 * This method is used to fetch Agreement Clauses Based On Id.
	 * @param agreementClauses - agreementClauses
	 * @return Object of agreementClauses.
	 */
	public AgreementClauses fetchAgreementClausesBasedOnId(Integer agreementClauseId);

	/**
	 * This method is used to saveOrUpdateAgreementClauses.
	 * @param agreementClauses - agreementClauses
	 * @return Object of agreementClauses.
	 */
	public void deleteAgreementClauses(AgreementClauses agreementClause);

	/**
	 * This method is used to fetchAllClausesGroupBasedOnAgreementType.
	 * @param agreementTypeCode - agreementTypeCode
	 * @return List of AgreementClausesGroupMapping.
	 */
	public List<AgreementClausesGroupMapping> fetchAllClausesGroupBasedOnAgreementType(String agreementTypeCode);

	/**
	 * This method is used to getAgreementTypeCodeByAgreementId.
	 * @param agreementRequestId - agreementRequestId
	 * @return String value.
	 */
	public String getAgreementTypeCodeByAgreementId(Integer agreementRequestId);

	/**
	 * This method is used to getAgreementAgreementAttachmentBasedOnParams.
	 * @param agreementRequestId - agreementRequestId
	 * @param versionNumber - versionNumber
	 * @return AgreementAttachment object.
	 */
	public AgreementAttachment getAgreementAgreementAttachmentBasedOnParams(Integer agreementRequestId, Integer versionNumber);


	/**
	 * This method is used to fetch agreement status based on status code.
	 * @param statusCode - statusCode.
	 * @return An AgreementStatus object.
	 */
	public AgreementWorkflowStatus fetchAgreementWorkflowStatusByStatusCode(String workflowStatusCode);

	/**
	 * This method is used to fetchAgreementClausesBasedOnParams.
	 * @param agreementRequestId - agreementRequestId.
	 * @param clausesGroupCode - clausesGroupCode.
	 * @return An AgreementStatus object.
	 */
	public List<AgreementClauses> fetchAgreementClausesBasedOnParams(Integer agreementRequestId,
			Integer clausesGroupCode);

	/**
	 * This method is used to fetchAgreementNoteBasedOnAgreementId.
	 * @param agreementRequestId - agreementRequestId.
	 * @return An AgreementStatus object.
	 */
	public List<AgreementNote> fetchAgreementNoteBasedOnAgreementId(Integer agreementRequestId);

	/**
	 * This method is used to get max version agreement template based on agreementTypeCode.
	 * @param agreementTypeCode - agreementTypeCode
	 * @return version number.
	 */
	public Integer fetchMaxVersionAgreementTemplateBasedOnParams(String agreementTypeCode);

	/**
	 * This method is used to get agreement type template base on params.
	 * @param agreementTypeCode - agreementTypeCode
	 * @param versionNumber - versionNumber
	 * @return AgreementTypeTemplate object.
	 */
	public AgreementTypeTemplate getAgreementTypeTemplateBasedOnParams(String agreementTypeCode, Integer versionNumber);

	/**
	 * This method is used to fetchAgreementSponsorContactsBasedOnId.
	 * @param agreementRequestId - agreementRequestId.
	 * @return An AgreementStatus object.
	 */
	public AgreementSponsorContact fetchAgreementSponsorContactsBasedOnId(Integer agreementSponsorContactId);

	/**
	 * This method is used to getAgreementSponsorContacts.
	 * @param agreementSponsorId - agreementSponsorId.
	 * @return A List of AgreementSponsorContact.
	 */
	public List<AgreementSponsorContact> getAgreementSponsorContacts(Integer agreementSponsorId);

	/**
	 * This method is used to getAgreementSponsorContacts.
	 * @param agreementSponsorId - agreementSponsorId.
	 * @return A List of AgreementSponsorContact.
	 */
	public ClausesBank saveOrUpdateClausesBank(ClausesBank clausesBank);

	/**
	 * This method is used to getAgreementSponsorContacts.
	 * @param agreementSponsorId - agreementSponsorId.
	 * @return A List of AgreementSponsorContact.
	 */
	public List<ClausesBank> getAllClausesBank();

	/**
	 * This method is used to getAgreementSponsorContacts.
	 * @param agreementSponsorId - agreementSponsorId.
	 * @return A List of AgreementSponsorContact.
	 */
	public void deleteClausesById(ClausesBank clausesCode);

	/**
	 * This method is used to getAgreementSponsorContacts.
	 * @param agreementSponsorId - agreementSponsorId.
	 * @return A List of AgreementSponsorContact.
	 */
	public ClausesBank fetchClausesBankById(Integer clausesCode);

	/**
	 * This method is used to fetch Agreement Note OnId.
	 * @param agreementSponsorId - agreementSponsorId.
	 * @return A List of AgreementSponsorContact.
	 */
	public AgreementNote fetchAgreementNoteById(Integer agreementNoteId);

	/**
	 * this method is used for fetchAgreementNoteAttachmentById
	 * @param attachmentId
	 * @return Object of AgreementNoteAttachment
	 */
	public AgreementNoteAttachment fetchAgreementNoteAttachmentById(Integer attachmentId);

	/**
	 * this method is used for delete Agreement note attachment
	 * @param agreementNoteAttachment - Object of AgreementNoteAttachment
	 */
	public void deleteAgreementNoteAttachment(AgreementNoteAttachment agreementNoteAttachment);

	/**
	 * This method is used to fetchAgreementPeopleTypes.
	 * @return A List of AgreementPeopleType.
	 */
	public List<AgreementPeopleType> fetchAgreementPeopleTypes();

	/**
	 * this method is used for saveOrUpdateAgreementPeople
	 * @param agreementPeople - Object of AgreementPeople
	 * @return Object of AgreementPeople
	 */
	public AgreementPeople saveOrUpdateAgreementPeople(AgreementPeople agreementPeople);

	/**
	 * this method is used for saveOrUpdateAgreementPeople
	 * @param agreementPeople - Object of AgreementPeople
	 * @return Object of AgreementPeople
	 */
	public List<AgreementPeople> getAllAgreementPeople(Integer agreementRequestId);

	/**
	 * this method is used for saveOrUpdateAgreementPeople
	 * @param agreementPeople - Object of AgreementPeople
	 */
	public void deleteAgreementPeople(AgreementPeople agreementPeople);

	/**
	 * this method is used for saveOrUpdateAgreementPeople
	 * @param agreementPeople - Object of AgreementPeople
	 * @return Object of AgreementPeople
	 */
	public AgreementPeople getAgreementPeople(Integer agreementPeopleId);

	/**
	 * this method is used for getSectionCodesBasedOnRights
	 * @param availableRights - List of availableRights
	 * @return List of section Codes
	 */
	public List<String> getSectionCodesBasedOnRights(List<String> availableRights);

	/**
	 * this method is used for getFieldCodesBasedOnSectionCodes
	 * @param section - sectionCode
	 * @return List of field Codes
	 */
	public List<String> getFieldCodesBasedOnSectionCodes(String section);

	/**
	 * This method is used to fetchAgreementNoteBasedOnAgreementId.
	 * @param actionLogId - ActionLogId.
	 * @return List of AgreementNote .
	 */
	public List<AgreementNote> fetchAgreementNoteBasedOnActionLogId(Integer actionLogId);

	/**
	 * This method is used to get All Agreement PlaceHolders.
	 * @return List of AgreementPlaceHolder.
	 */
	public List<AgreementPlaceHolder> getAllActiveAgreementPlaceHolders();

	/**
	 * This method is used to get Questionnaire Attachments.
	 * @return List of Objects.
	 */
	public List<Object[]> getQuestionnaireAttachments(Integer moduleCode, Integer subModuleCode, String moduleItemKey);

	/**
	 * This method is used to get Agreement People Type By Id.
	 * @return List of AgreementPeopleType.
	 */
	public AgreementPeopleType getAgreementPeopleTypeById(Integer peopleTypeId);

	/**
	 * This method is used to get Agreement People Type By Id.
	 * @return List of AgreementPeopleType.
	 */
	public AgreementTypeHistory saveOrUpdateAgreementTypeHistory(AgreementTypeHistory agreementTypeHistory);

	/**
	 * This method is used to getAgreementTypeHistory.
	 * @return Set of Agreement Type Code.
	 */
	public List<AgreementTypeHistory> getAgreementTypeHistory(Integer agreementRequestId);

	/**
	 * This method is used to getAgreementName.
	 * @return Agreement Name.
	 */
	public String getAgreementName(Integer agreementRequestId);

	/**
	 * This method is used to fetchSponsorRoles.
	 * @return List of SponsorRole.
	 */
	public List<SponsorRole> fetchSponsorRoles();

	/**
	 * This method is used to fetchAgreementSponsorTypes.
	 * @return List of SponsorRole.
	 */
	public List<AgreementSponsorType> fetchAgreementSponsorTypes();

	/**
	 * This method is used to getPrimaryOrganisationName.
	 * @param agreementRequestId - agreementRequestId
	 * @return Primary organisation Name.
	 */
	public String getPrimaryOrganisationName(Integer agreementRequestId);

	/**
	 * This method is used to getPrimaryOrganisationName.
	 * @param agreementRequestId - agreementRequestId
	 * @return Primary organisation Name.
	 */
	public AgreementPeople getAgreementPI(Integer agreementRequestId);

	/**
	 * This method is used to get Triage Header Id Based On AgreementId.
	 * @param agreementRequestId - agreementRequestId
	 * @param moduleCode - moduleCode 
	 * @return Triage Header ID.
	 */
	public Integer getTriageHeaderIdBasedOnAgreementId(Integer agreementHeaderId, Integer moduleCode);

	/**
	 * This method is used to get getAgreementAdminByAgreementId.
	 * @param agreementRequestId - agreementRequestId
	 * @param personId - personId
	 * @param peopleTypeId - peopleTypeId
	 * @return boolean response.
	 */
	public AgreementPeople checkPersonExist(Integer agreementRequestId, String personId, Integer peopleTypeId);

	/**
	 * This method is used to fetch all agreement review types.
	 * @return List of agreement review types.
	 */
	public List<AgreementReviewType> fetchAgreementReviewTypes();

	/**
	 * This method is used to fetch agreement review type.
	 * @return Object of agreement review types.
	 */
	public AgreementReviewType fetchAgreementReviewTypes(String agreementReviewTypeCode);

	/**
	 * This method is used to save linked module details.
	 * @param associationDetail
	 * @return
	 */
	public AgreementAssociationDetail saveOrUpdateAgreementAssociationDetail(AgreementAssociationDetail associationDetail);

	/**
	 * This method is used to save agreement and linked module details.
	 * @param moduleLink
	 * @return
	 */
	public AgreementAssociationLink saveOrUpdateAgreementAssociationLink(AgreementAssociationLink moduleLink);

	/**
	 * This method is used check for agreement Sponsor based on sponsor code.
	 * @param sponsorCode
	 * @return
	 */
	public Boolean checkForAgreementSponsor(String sponsorCode);

	/**
	 * This module is used to get link details.
	 * @param agreementRequestId
	 * @return
	 */
	public List<AgreementAssociationLink> getExternalModuleLinkDetails(Integer agreementRequestId);

	/**
	 * This method is used to delete the agreement association link detail.
	 * @param moduleCode
	 * @param moduleItemKey
	 * @param agreementRequestId
	 */
	public void deleteAgreementLink(Integer moduleCode, String moduleItemKey, Integer agreementRequestId);

	/**
	 * This method is used to get agreement type based on type code.
	 * @param typeCode
	 * @return
	 */
	public AgreementType fetchAgreementTypeByTypeCode(String typeCode);

	/**
	 * This method is used to getPrimaryOrganisationCode.
	 * @param agreementRequestId - agreementRequestId
	 * @return Primary organisation Code.
	 */
	public String getPrimaryOrganisationCode(Integer agreementRequestId);

	/**
	 * @param agreementRequestId
	 * @return
	 */
	public Boolean checkIfExternalModuleLinked(Integer agreementRequestId);

	/**
	 * This method is used to delete agreement clausees
	 * @param clausesGroupCode
	 */
	public void deleteAgreementClausesByClauseGroupCode(Integer clausesGroupCode);

	/**
	 * This method is used to delete Agreement clause type.
	 * @param clausesGroupCode
	 */
	public void deleteAgreementTypeClausesMappingByClauseGroupCode(Integer clausesGroupCode);

	/**
	 * This method is used to delete clause group.
	 * @param clausesGroupCode
	 */
	public void deleteClausesGroup(Integer clausesGroupCode);

	/**
	 * This method is used to get contract admin detail based on agreement Id and people type Id.
	 * @param agreementRequestId
	 * @param peopleTypeId
	 * @return
	 */
	public List<AgreementPeople> getAgreementPeopleByTypeId(Integer agreementRequestId, Integer peopleTypeId);

	/**
	 * This method is used to get narrative status based on id.
	 * @param code - Id of the AgreementAttachmentStatus.
	 * @return AgreementAttachmentStatus.
	 */
	public AgreementAttachmentStatus getAgreementAttachmentStatusByCode(String code);

	/**
	 * This method is used to get agreement sponsor type by sponsorTypeCode.
	 * @param sponsorTypeCode
	 * @return
	 */
	public AgreementSponsorType getAgreementSponsorTypeByTypeCode(String sponsorTypeCode);

	/**
	 * This method is used to updateAttachmentDetails.
	 * @param description
	 * @param agreementAttachmentId
	 */
	public void updateAttachmentDetails(String description, Integer agreementAttachmentId);
	
	/**
	 * This method is used to delete agreement record.
	 * @param agreementRequestId
	 */
	String deleteAgreement(Integer agreementRequestId);

}
