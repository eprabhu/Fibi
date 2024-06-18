package com.polus.fibicomp.coi.clients;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import com.polus.fibicomp.coi.clients.model.ApplicableFormRequest;
import com.polus.fibicomp.coi.clients.model.ApplicableFormResponse;
import com.polus.fibicomp.coi.clients.model.BlankFormRequest;
import com.polus.fibicomp.coi.clients.model.BlankFormResponse;
import com.polus.fibicomp.coi.clients.model.FormRequest;
import com.polus.fibicomp.coi.clients.model.FormResponse;

/**
 * We can customize here if need
 * <a href="https://polussoftware0-my.sharepoint.com/:w:/g/personal/ajin_vs_polussolutions_com/EbsVHRTRLuZDv7iLDbM9Nc0BHYb5tc-juKji954sbX7JJQ?e=2Ivthl"> documentation</a>
 */
@FeignClient("FIBI-FORM-BUILDER")
public interface FormBuilderClient {

    @PostMapping("/formbuilder/getApplicableForms")
    ResponseEntity<ApplicableFormResponse> getApplicableForms(@RequestBody ApplicableFormRequest request);

    @PostMapping("/formbuilder/getForm")
    ResponseEntity<FormResponse> getForm(@RequestBody FormRequest request);

    @PostMapping("/formbuilder/getBlankForm")
    ResponseEntity<BlankFormResponse> getBlankForm(@RequestBody BlankFormRequest request);


}
