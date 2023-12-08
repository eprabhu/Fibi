package com.polus.formbuilder.reviewcomments.serviceClients;

import com.polus.formbuilder.reviewcomments.modals.ReviewComment;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

//url = "${openfeign.client.config.reviewCommentsClient.url}"
@FeignClient(name = "reviewCommentsClient")
public interface ReviewCommentsClient {

    @PostMapping
    ResponseEntity<Object> saveOrUpdateReviewComment(@RequestParam(value = "files", required = false) MultipartFile[] files,
                                                            @RequestParam("formDataJson") String formDataJson);

    @PostMapping("/fetch")
    ResponseEntity<Object> fetchReviewComments(@RequestBody ReviewComment reviewCommentsDto);

    @DeleteMapping(value = "/{reviewCommentId}/{moduleCode}")
    ResponseEntity<Object> deleteReviewComment(@PathVariable(value = "reviewCommentId") final Integer reviewCommentId,
                                               @PathVariable(value = "moduleCode") Integer moduleCode);

    @DeleteMapping(value = "/attachment/{attachmentId}/{moduleCode}")
    ResponseEntity<Object> deleteReviewAttachment(@PathVariable(value = "attachmentId") Integer attachmentId,
                                                  @PathVariable(value = "moduleCode") Integer moduleCode);

    @GetMapping(value = "/downloadAttachment/{moduleCode}")
    ResponseEntity<byte[]> downloadReviewAttachment(@RequestHeader("attachmentId") Integer attachmentId,
                                                    @PathVariable(value = "moduleCode") Integer moduleCode);
}
