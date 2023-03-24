package com.polus.fibicomp.grantcall.schedule;

import java.sql.Timestamp;
import java.text.ParseException;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.hibernate.Session;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.service.GrantCallService;
import com.polus.fibicomp.grantcall.vo.GrantCallVO;
import com.polus.fibicomp.notification.email.dao.EmailMaintenanceDao;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.notification.pojo.NotificationType;

@Component
@Transactional
public class GrantCallStatusScheduler {

	protected static Logger logger = LogManager.getLogger(GrantCallStatusScheduler.class.getName());

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private HibernateTemplate hibernateTemplate;

	@Autowired
	private EmailMaintenanceDao emailMaintenanceDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private GrantCallService grantCallService;

	private static final String GRANT_STATUS_CODE = "grantStatusCode";

	private static final String MESSAGE = "GrantCallId : {}";

	@Scheduled(cron = "${grantCall.updateStatusClose.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void updateGrantCallStatus() throws ParseException {
		Timestamp currentDate = commonDao.getCurrentDateBasedOnTimeZone(Constants.CRON_JOB_TIMEZONE);
		logger.info("GrantCall Closing Status Scheduler Starts at {}" , currentDate);
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCall> query = builder.createQuery(GrantCall.class);
		Root<GrantCall> parameter = query.from(GrantCall.class);
		Predicate predicate1 = builder.lessThan(parameter.get("closingDate"), currentDate);
		Predicate predicate2 = builder.equal(parameter.get(GRANT_STATUS_CODE), Constants.GRANT_CALL_STATUS_CODE_OPEN);
		query.where(builder.and(predicate1, predicate2));
		List<GrantCall> grantCall = session.createQuery(query).getResultList();
		for (GrantCall grantCallValue : grantCall) {
			logger.info(MESSAGE, grantCallValue.getGrantCallId());
			grantCallValue.setGrantStatusCode(Constants.GRANT_CALL_STATUS_CODE_CLOSED);
			grantCallValue.setGrantCallStatus(grantCallDao.fetchStatusByStatusCode(Constants.GRANT_CALL_STATUS_CODE_CLOSED));
			grantCallDao.saveOrUpdateGrantCall(grantCallValue);
		}
	}

	@Scheduled(cron = "${grantCall.updateStatusOpen.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void updateGrantCallStatusToOpen() {
		Timestamp currentDate = committeeDao.getCurrentTimestamp();
		logger.info("GrantCall Open Status Scheduler Starts at {}", currentDate);
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCall> query = builder.createQuery(GrantCall.class);
		Root<GrantCall> parameter = query.from(GrantCall.class);
		Predicate predicate1 = builder.lessThanOrEqualTo(parameter.get("openingDate"), currentDate);
		Predicate predicate2 = builder.equal(parameter.get(GRANT_STATUS_CODE), Constants.GRANT_CALL_STATUS_CODE_TENTATIVE);
		Predicate predicate3 = builder.equal(parameter.get("grantCallType").get("categoryCode"), Constants.GRANT_CALL_TYPE_INTERNAL);
		query.where(builder.and(predicate1, predicate2, predicate3));
		List<GrantCall> grantCall = session.createQuery(query).getResultList();
		for (GrantCall grantCallValue : grantCall) {
			logger.info(MESSAGE, grantCallValue.getGrantCallId() + " Opens");
			grantCallValue.setGrantStatusCode(Constants.GRANT_CALL_STATUS_CODE_OPEN);
			grantCallValue.setGrantCallStatus(grantCallDao.fetchStatusByStatusCode(Constants.GRANT_CALL_STATUS_CODE_OPEN));
			grantCallDao.saveOrUpdateGrantCall(grantCallValue);
			NotificationType notificationType = emailMaintenanceDao.fetchNotificationById(Constants.GRANT_CALL_OPENING_NOTIFICATION_CODE);
			Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
			GrantCallVO grantCallVO = new GrantCallVO();
			grantCallVO.setGrantCall(grantCallValue);
			grantCallService.sendGrantCallNotification(grantCallVO, notificationType.getNotificationTypeId(), dynamicEmailrecipients);
		}
	}

	@Scheduled(cron = "${grantCall.openRemainder.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void grantCallOpenReminder() {
		Timestamp currentDate = committeeDao.getCurrentTimestamp();
		logger.info("GrantCall Open Reminder Status Scheduler Starts at {}", currentDate);
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCall> query = builder.createQuery(GrantCall.class);
		Root<GrantCall> parameter = query.from(GrantCall.class);
		Date dateTwoWeeksAgo = new DateTime(new Date()).minusDays(14).toDate();
		Predicate predicate1 = builder.lessThanOrEqualTo(parameter.get("openingDate"), new Timestamp(dateTwoWeeksAgo.getTime()));
		Predicate predicate2 = builder.equal(parameter.get(GRANT_STATUS_CODE), Constants.GRANT_CALL_STATUS_CODE_TENTATIVE);
		Predicate predicate3 = builder.equal(parameter.get("grantCallType").get("categoryCode"), Constants.GRANT_CALL_TYPE_INTERNAL);
		query.where(builder.and(predicate1, predicate2, predicate3));
		List<GrantCall> grantCall = session.createQuery(query).getResultList();
		for (GrantCall grantCallValue : grantCall) {
			Date startDate = new Date(grantCallValue.getOpeningDate().getTime());
			long difference = dateTwoWeeksAgo.getTime() - startDate.getTime();
			if (TimeUnit.DAYS.convert(difference, TimeUnit.MILLISECONDS) == 0) {
				logger.info(MESSAGE, grantCallValue.getGrantCallId() + " Opens after 2 weeks");
				NotificationType notificationType = emailMaintenanceDao.fetchNotificationById(Constants.GRANT_CALL_OPENING_SCHEDULER_NOTIFICATION_CODE);
				Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
				GrantCallVO grantCallVO = new GrantCallVO();
				grantCallVO.setGrantCall(grantCallValue);
				grantCallService.sendGrantCallNotification(grantCallVO, notificationType.getNotificationTypeId(), dynamicEmailrecipients);
			}
		}
	}

	@Scheduled(cron = "${grantCall.closeRemainder.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void grantCallCloseReminder() {
		Timestamp currentDate = committeeDao.getCurrentTimestamp();
		logger.info("GrantCall Close Reminder Status Scheduler Starts at {}", currentDate);
		Session session = hibernateTemplate.getSessionFactory().getCurrentSession();
		CriteriaBuilder builder = session.getCriteriaBuilder();
		CriteriaQuery<GrantCall> query = builder.createQuery(GrantCall.class);
		Root<GrantCall> parameter = query.from(GrantCall.class);
		Predicate predicate1 = builder.greaterThanOrEqualTo(parameter.get("closingDate"), currentDate);
		Predicate predicate2 = builder.equal(parameter.get(GRANT_STATUS_CODE), Constants.GRANT_CALL_STATUS_CODE_OPEN);
		Predicate predicate3 = builder.equal(parameter.get("grantCallType").get("categoryCode"), Constants.GRANT_CALL_TYPE_INTERNAL);
		query.where(builder.and(predicate1, predicate2, predicate3));
		List<GrantCall> grantCall = session.createQuery(query).getResultList();
		for (GrantCall grantCallValue : grantCall) {
			Date dateOneWeeksAgo = new DateTime(grantCallValue.getClosingDate()).minusDays(7).toDate();
			long difference = dateOneWeeksAgo.getTime() - currentDate.getTime();
			if (TimeUnit.DAYS.convert(difference, TimeUnit.MILLISECONDS) == 0) {
				logger.info(MESSAGE, grantCallValue.getGrantCallId() + " closes after 1 week");
				NotificationType notificationType = emailMaintenanceDao.fetchNotificationById(Constants.GRANT_CALL_CLOSING_NOTIFICATION_CODE);
				GrantCallVO grantCallVO = new GrantCallVO();
				grantCallVO.setGrantCall(grantCallValue);
				grantCallService.sendGrantCallNotification(grantCallVO, notificationType.getNotificationTypeId(), new HashSet<>());
			}
		}
	}

}
