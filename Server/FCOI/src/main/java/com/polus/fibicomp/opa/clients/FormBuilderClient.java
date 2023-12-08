package com.polus.fibicomp.opa.clients;

import com.polus.fibicomp.opa.clients.model.*;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

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
