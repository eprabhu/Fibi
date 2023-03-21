package com.polus.fibicomp.externalreview.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.fibicomp.externalreview.pojo.ExtReviewAttachments;
import com.polus.fibicomp.externalreview.pojo.ExtReviewQuestionnaire;
import com.polus.fibicomp.externalreview.pojo.ExternalReview;
import com.polus.fibicomp.externalreview.service.ExternalReviewService;
import io.jsonwebtoken.lang.Collections;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.Valid;
import java.io.IOException;
import java.util.List;

@RestController
@RequestMapping("/externalReview")
public class ExternalReviewController {

    @Autowired
    ExternalReviewService externalReviewService;

    @PostMapping("/questionnaire")
    public ResponseEntity<Object> saveExtReviewQuestionnaire(@RequestBody @Valid ExtReviewQuestionnaire reviewQuestionnaire) {
        return externalReviewService.saveExtReviewQuestionnaire(reviewQuestionnaire);
    }

    @DeleteMapping("/questionnaire/{extReviewQuestionnaireId}")
    public ResponseEntity<Object> deleteExtReviewQuestionnaire(@PathVariable("extReviewQuestionnaireId") Integer extReviewQuestionnaireId) {
        return externalReviewService.deleteExtReviewQuestionnaire(extReviewQuestionnaireId);
    }

    @GetMapping("/questionnaire/{extReviewId}")
    public ResponseEntity<Object> getExtReviewQuestionnaire(@PathVariable("extReviewId") Integer extReviewId) {
        return externalReviewService.getExtReviewQuestionnaire(extReviewId);
    }

    @GetMapping("/serviceType")
    public ResponseEntity<Object> getExtReviewServiceType() {
        return externalReviewService.getExtReviewServiceType();
    }

    @GetMapping("/statuses")
    public ResponseEntity<Object> getExtReviewStatus() {
        return externalReviewService.getExtReviewStatus();
    }

    @PostMapping
    public ResponseEntity<Object> createExternalReview(@RequestBody ExternalReview externalReview) {
        return externalReviewService.createExternalReview(externalReview);
    }

    @PutMapping
    public ResponseEntity<Object> updateExternalReview(@RequestBody ExternalReview externalReview) {
        return externalReviewService.updateExternalReview(externalReview);
    }

    @PostMapping("/{moduleItemCode}")
    public ResponseEntity<Object> getExternalReview(@RequestBody ExternalReview externalReview, @PathVariable("moduleItemCode") Integer moduleItemCode) {
        return externalReviewService.getExternalReview(externalReview, moduleItemCode);
    }

    @GetMapping("/scoringCriteria/{extReviewID}")
    public ResponseEntity<Object> getExtReviewScoringCriteria(@PathVariable("extReviewID") Integer extReviewID) {
        return externalReviewService.getExtReviewScoringCriteria(extReviewID);
    }

    @GetMapping("/grantCallProposalAttachments/{proposalID}")
    public ResponseEntity<Object> getProposalGrantCallAttachments(@PathVariable("proposalID") Integer proposalID) {
        return externalReviewService.getProposalGrantCallAttachments(proposalID);
    }


    @PostMapping(value = "/attachments")
    public ResponseEntity<Object> saveExtReviewAttachments(@RequestParam(value = "files", required = false) MultipartFile[] files,
                                                           @RequestParam(name = "fileFormData", required = false) String fileFormData,
                                                           @RequestParam(name = "grantCallProposalFormData", required = false) String grantCallProposalFormData) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        List<ExtReviewAttachments> extReviewAttachment = null;
        if (fileFormData != null && !fileFormData.isEmpty())
            extReviewAttachment = Collections.arrayToList(mapper.readValue(fileFormData, ExtReviewAttachments[].class));
        List<ExtReviewAttachments> grantCallProposalAttachments = null;
        if (grantCallProposalFormData != null && !grantCallProposalFormData.isEmpty())
            grantCallProposalAttachments = Collections.arrayToList(mapper.readValue(grantCallProposalFormData, ExtReviewAttachments[].class));
        return externalReviewService.saveExtReviewAttachments(extReviewAttachment, grantCallProposalAttachments, files);
    }

    @GetMapping("/attachments/{extReviewID}")
    public ResponseEntity<Object> getExtReviewAttachments(@PathVariable("extReviewID") Integer extReviewID) {
        return externalReviewService.getExtReviewAttachments(extReviewID);
    }

    @GetMapping("/attachmentTypes")
    public ResponseEntity<Object> getExtReviewAttachmentTypes() {
        return externalReviewService.getExtReviewAttachmentTypes();
    }

    @DeleteMapping("/attachments/{extReviewAttachmentId}")
    public ResponseEntity<Object> deleteExtReviewAttachment(@PathVariable("extReviewAttachmentId") Integer extReviewAttachmentId) {
        return externalReviewService.deleteExtReviewAttachment(extReviewAttachmentId);
    }

    @PutMapping("/attachments")
    public ResponseEntity<Object> updateExtReviewAttachments(@RequestBody MultipartFile file, @RequestParam("fileFormData") String fileFormData) throws IOException {
        ObjectMapper mapper = new ObjectMapper();
        ExtReviewAttachments extReviewAttachment = mapper.readValue(fileFormData, ExtReviewAttachments.class);
        return externalReviewService.updateExtReviewAttachments(file, extReviewAttachment);
    }

    @GetMapping("/attachment/download/{extReviewAttachmentId}")
    public ResponseEntity<byte[]> downloadExtReviewAttachments(@PathVariable("extReviewAttachmentId") Integer extReviewAttachmentId) {
        return externalReviewService.downloadExtReviewAttachments(extReviewAttachmentId);
    }

    @PatchMapping()
    public ResponseEntity<Object> sendForReview(@RequestParam("extReviewID") Integer extReviewID) {
        return externalReviewService.sendForReview(extReviewID);
    }
}
