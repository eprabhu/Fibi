package com.polus.fibicomp.opa.service;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.opa.dao.OPADao;
import com.polus.fibicomp.opa.dao.OPAReviewDao;
import com.polus.fibicomp.opa.dto.OPAReviewDto;
import com.polus.fibicomp.opa.pojo.OPAReview;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.security.AuthenticatedUser;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Service
@Transactional
public class OPAReviewServiceImpl implements OPAReviewService {

    @Autowired
    private OPAReviewDao reviewDao;

    @Autowired
    private OPADao opaDao;

    @Autowired
    private PersonDao personDao;

    @Autowired
    private CommonDao commonDao;

    @Override
    public ResponseEntity<Object> saveOrUpdateOPAReview(OPAReview opaReview) {
        Timestamp updateTimestamp = commonDao.getCurrentTimestamp();
        if (opaReview.getOpaReviewId() == null) {
            opaReview.setCreateUser(AuthenticatedUser.getLoginUserName());
            opaReview.setUpdateTimestamp(updateTimestamp);
            reviewDao.saveOrUpdate(opaReview);
            opaReview.setOpaReviewId(opaReview.getOpaReviewId());
            opaReview.setUpdateUserFullName(AuthenticatedUser.getLoginUserFullName());
        } else {
            updateTimestamp = reviewDao.updateOPAReview(opaReview);
        }
        OPAReviewDto reviewDto = new OPAReviewDto();
        BeanUtils.copyProperties(opaReview, reviewDto);
        if (reviewDao.numberOfReviewOfStatuesIn(opaReview.getOpaDisclosureId(), Arrays.asList(Constants.OPA_REVIEW_ASSIGNED,
                Constants.OPA_REVIEW_IN_PROGRESS)) == 1) {
            opaDao.updateOPADisclosureStatuses(opaReview.getOpaDisclosureId(), updateTimestamp, Constants.OPA_DISCLOSURE_STATUS_REVIEW_ASSIGNED, null);
            reviewDto.setOpaDisclosureStatusType(opaDao.getOPADisclosureStatusType(Constants.OPA_DISCLOSURE_STATUS_REVIEW_ASSIGNED));
        } else if (reviewDao.numberOfReviewOfStatuesIn(opaReview.getOpaDisclosureId(), Arrays.asList(Constants.OPA_REVIEW_ASSIGNED,
                Constants.OPA_REVIEW_IN_PROGRESS)) == 0) {
            opaDao.updateOPADisclosureStatuses(opaReview.getOpaDisclosureId(), updateTimestamp, Constants.OPA_DISCLOSURE_STATUS_REVIEW_COMPLETED, null);
            reviewDto.setOpaDisclosureStatusType(opaDao.getOPADisclosureStatusType(Constants.OPA_DISCLOSURE_STATUS_REVIEW_COMPLETED));
        } else {
            opaDao.updateOPADisclosureUpDetails(opaReview.getOpaDisclosureId(), updateTimestamp);
            reviewDto.setOpaDisclosureStatusType(opaDao.getOPADisclosureStatusType(Constants.OPA_DISCLOSURE_STATUS_REVIEW_ASSIGNED));
        }
        return new ResponseEntity<>(reviewDto, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> getOPAReview(Integer opaDisclosureId) {
        List<OPAReviewDto> reviews = new ArrayList<>();
        reviewDao.fetchAllOPAReviewByDisId(opaDisclosureId).forEach(review -> {
            OPAReviewDto reviewDto = new OPAReviewDto();
            BeanUtils.copyProperties(review, reviewDto);
            reviewDto.setAssigneePersonName(personDao.getPersonFullNameByPersonId(review.getAssigneePersonId()));
            reviews.add(reviewDto);
        });
        return new ResponseEntity<>(reviews, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> startOPAReview(Integer opaReviewId) {
        Timestamp timestamp = reviewDao.updateReviewStatus(opaReviewId, Constants.OPA_REVIEW_IN_PROGRESS);
        OPAReview opaReview = reviewDao.getOPAReview(opaReviewId);
        opaDao.updateOPADisclosureUpDetails(opaReview.getOpaDisclosureId(), timestamp);
        OPAReviewDto reviewDto = new OPAReviewDto();
        BeanUtils.copyProperties(opaReview, reviewDto, "reviewStatusType", "reviewLocationType");
        reviewDto.setOpaDisclosureStatusType(opaDao.getOPADisclosureStatusType(Constants.OPA_DISCLOSURE_STATUS_REVIEW_ASSIGNED));
        return new ResponseEntity<>(reviewDto, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> completeOPAReview(Integer opaReviewId) {
        Timestamp timestamp = reviewDao.updateReviewStatus(opaReviewId, Constants.OPA_REVIEW_COMPLETED);
        OPAReview opaReview = reviewDao.getOPAReview(opaReviewId);
        OPAReviewDto reviewDto = new OPAReviewDto();
        BeanUtils.copyProperties(opaReview, reviewDto, "reviewStatusType", "reviewLocationType");
        if (reviewDao.numberOfReviewOfStatuesIn(opaReview.getOpaDisclosureId(), Arrays.asList(Constants.OPA_REVIEW_ASSIGNED,
                Constants.OPA_REVIEW_IN_PROGRESS)) == 0) {
            opaDao.updateOPADisclosureStatuses(opaReview.getOpaDisclosureId(), timestamp, Constants.OPA_DISCLOSURE_STATUS_REVIEW_COMPLETED, null);
            reviewDto.setOpaDisclosureStatusType(opaDao.getOPADisclosureStatusType(Constants.OPA_DISCLOSURE_STATUS_REVIEW_COMPLETED));
        } else {
            reviewDto.setOpaDisclosureStatusType(opaDao.getOPADisclosureStatusType(Constants.OPA_DISCLOSURE_STATUS_REVIEW_ASSIGNED));
        }
        return new ResponseEntity<>(reviewDto, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<Object> deleteOPAReview(Integer opaReviewId) {
        OPAReview opaReview = reviewDao.getOPAReview(opaReviewId);
        OPAReviewDto reviewDto = new OPAReviewDto();
        BeanUtils.copyProperties(opaReview, reviewDto, "reviewStatusType", "reviewLocationType");
        //TODO need to delete other review related tables
        reviewDao.deleteOPAReview(opaReviewId);
        if (reviewDao.numberOfReviewOfStatuesIn(opaReview.getOpaDisclosureId(), Arrays.asList(Constants.OPA_REVIEW_ASSIGNED,
                Constants.OPA_REVIEW_IN_PROGRESS)) == 0) {
            opaDao.updateOPADisclosureStatuses(opaReview.getOpaDisclosureId(), commonDao.getCurrentTimestamp() , Constants.OPA_DISCLOSURE_STATUS_REVIEW_COMPLETED, null);
            reviewDto.setOpaDisclosureStatusType(opaDao.getOPADisclosureStatusType(Constants.OPA_DISCLOSURE_STATUS_REVIEW_COMPLETED));
        } else {
            reviewDto.setOpaDisclosureStatusType(opaDao.getOPADisclosureStatusType(Constants.OPA_DISCLOSURE_STATUS_REVIEW_ASSIGNED));
        }
        return new ResponseEntity<>(reviewDto, HttpStatus.OK);
    }
}
