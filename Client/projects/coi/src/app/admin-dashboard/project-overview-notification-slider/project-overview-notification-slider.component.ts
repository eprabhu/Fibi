import { Component, EventEmitter, HostListener, Input, OnInit, Output } from '@angular/core';
import { getFormattedSponsor, openCoiSlider } from '../../common/utilities/custom-utilities';
import { ProjectOverviewService } from '../project-overview.service';
import { CommonService } from '../../common/services/common.service';
import * as DecoupledEditor from '@ckeditor/ckeditor5-build-decoupled-document';
import { EDITOR_CONFIURATION, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from 'projects/fibi/src/app/app-constants';
import { GetNotificationsRO, NotificationObject } from '../admin-dashboard.interface';
import { Subscription } from 'rxjs';
import { removeUnwantedTags } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { ElasticConfigService } from '../../common/services/elastic-config.service';

@Component({
    selector: 'app-project-overview-notification-slider',
    templateUrl: './project-overview-notification-slider.component.html',
    styleUrls: ['./project-overview-notification-slider.component.scss']
})
export class ProjectOverviewNotificationSliderComponent implements OnInit {

    @Input() dataForNotificationSlider: any;
    @Input() keypersonIndex: number;
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
    getNotificationRO: GetNotificationsRO = new GetNotificationsRO();
    notificationObject: NotificationObject = new NotificationObject();
    defaultKeypersonDetails: {
        recipientName: string;
        recipientPersonId: string;
        recipientType: string;
    } = {
            recipientName: '',
            recipientPersonId: '',
            recipientType: 'TO',
        };
    getFormattedSponsor = getFormattedSponsor;
    public Editor = DecoupledEditor;
    editorConfig = EDITOR_CONFIURATION;
    comments = '';
    validationMap = new Map();
    notificationTemplates = [];
    templateOptions = 'EMPTY#EMPTY#false#false';
    defaultTemplateLabel = '';
    isCcViewable: boolean;
    isBccViewable: boolean;
    isEditorFocused = false;
    elasticSearchOptionsForTo: any = {};

    /**
     *  commenting the code of CC and BCC it will be enable form the next commit 
     */
    // elasticSearchOptionsForCc: any = {};
    // elasticSearchOptionsForBcc: any = {};
    clearField: String;
    pointOfContactObject: any = {};
    $subscriptions: Subscription[] = [];
    result: any;
    recipients: any = {};
    moduleName: string = 'TEST';
    send = null;


    constructor(public projectOverviewService: ProjectOverviewService, public commonService: CommonService,
        private _elasticConfig: ElasticConfigService
    ) { }

    ngOnInit() {
        setTimeout(() => {
            openCoiSlider('coi-project-notification-slider');
        });
        this.setElasticPerson();
        this.fetchAllNotifications();
        this.setDefaultKeypersonDetails();
        this.setDefaultDetails();
        this.notificationObject.recipients.push(this.defaultKeypersonDetails);
    }

    setDefaultDetails() {
        this.notificationObject.projectTypeCode = this.dataForNotificationSlider.projectDetails.projectTypeCode;
        this.notificationObject.projectId = this.dataForNotificationSlider.projectDetails.projectId;
        this.notificationObject.disclosureId = this.dataForNotificationSlider.keyPersonDetails[this.keypersonIndex].disclosureId;
    }

    setDefaultKeypersonDetails() {
        this.defaultKeypersonDetails.recipientName = this.dataForNotificationSlider.keyPersonDetails[this.keypersonIndex].keyPersonName;
        this.defaultKeypersonDetails.recipientPersonId = this.dataForNotificationSlider.keyPersonDetails[this.keypersonIndex].keyPersonId;
    }

    validateSliderClose() {
        this.clearNotifyFields();
        setTimeout(() => {
            this.closePage.emit();
        }, 500);
    }

    public onReady(editor) {
        editor.ui.getEditableElement().parentElement.insertBefore(
            editor.ui.view.toolbar.element,
            editor.ui.getEditableElement()
        );
    }

    validateMandatoryFields() {
        this.notificationObject.message = removeUnwantedTags(this.notificationObject.message);
        this.validationMap.clear();
        const TO_RECIPIENTS = this.notificationObject.recipients.map(e => e.recipientType === 'TO')
        if (TO_RECIPIENTS.length === 0) {
            this.validationMap.set('recipients', '* Please add Recipient');
        }
        if (!this.notificationObject.subject) {
            this.validationMap.set('subject', ' * Please select Subject  ');
        }
        if (!this.notificationObject.message) {
            this.validationMap.set('message', '* Please add Message');
        }
    }

    sendNotification() {
        this.validateMandatoryFields();
        if (this.validationMap.size === 0) {
            this.$subscriptions.push(
                this.projectOverviewService.sendNotification(this.notificationObject).subscribe(response => {
                    if (response.includes('successfully')) {
                        this.validateSliderClose();
                        this.clearNotifyFields();
                        this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Notification sent successfully.');
                    } else {
                        this.commonService.showToast(HTTP_ERROR_STATUS, 'Notification failed.');
                    }
                }, err => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Sending Notification failed. Please try again.');
                }));
        }
    }

    /**
   * This host listener is used to keep the background scroll fixed at the top at all times.
   */
    @HostListener('window:scroll')
    onScroll() {
        if (this.isEditorFocused) {
            window.scrollTo(0, 0);
        }
    }

    onEditorFocus() {
        this.isEditorFocused = true;
    }

    onEditorBlur() {
        this.isEditorFocused = false;
    }

    /**
  * used to get selected object from elastic search
  * @param {} value
  */
    selectedPOC(value) {
        if (value) {
            this.pointOfContactObject = {};
            this.pointOfContactObject.fullName = value.full_name;
            this.pointOfContactObject.personId = value.prncpl_id;
            this.elasticSearchOptionsForTo.defaultValue = '';
            //   this.elasticSearchOptionsForCc.defaultValue = '';
            //   this.elasticSearchOptionsForBcc.defaultValue = '';
            this.elasticSearchOptionsForTo = Object.assign({}, this.elasticSearchOptionsForTo);
            //   this.elasticSearchOptionsForCc = Object.assign({}, this.elasticSearchOptionsForCc);
            //   this.elasticSearchOptionsForBcc = Object.assign({}, this.elasticSearchOptionsForBcc);
            this.addPerson(this.pointOfContactObject);
        }
    }

    fetchAllNotifications() {
        this.notificationTemplates = [];
        this.notificationObject = new NotificationObject();
        this.$subscriptions.push(this.commonService.fetchAllNotifications(this.getNotificationRO).
            subscribe((data: any) => {
                if (data != null) {
                    this.notificationTemplates = data?.notificationTypes;
                    this.result = data;
                }
            }));
    }

    onTemplateSelect(event) {

        if (event) {
            this.notificationObject.message = event[0].message;
            this.notificationObject.subject = event[0].subject;
            this.notificationObject.description = event[0].description;
            this.notificationObject.notificationTypeId = event[0].notificationTypeId
        } else {
            this.notificationObject.message = null;
            this.notificationObject.subject = null;
            this.notificationObject.description = null;
        }
    }

    selectedRecipient(value: any): void {
        if (!this.notificationObject.recipients.find(recipient => recipient.recipientPersonId === value.prncpl_id && recipient.recipientType === this.send)) {
            this.selectedPOC(value);
        }
        else {
            this.setDuplicationValidation('* Person already added. Please choose a different person.');
            this.setElasticPerson();
            this.clearField = new String('true');
        }
    }

    addPerson(pointOfContactObject: any) {
        this.clearField = new String('true');
        if (pointOfContactObject.fullName != null || pointOfContactObject.fullName !== undefined) {
            this.recipients = this.addPersonDetails(pointOfContactObject, this.send);
            if (this.recipients) {
                this.notificationObject.recipients.push(Object.assign({}, this.recipients));
            }
            pointOfContactObject.fullName = null;
            pointOfContactObject.personId = null;
        }
    }

    addPersonDetails(pointOfContactObject, send) {
        this.recipients.recipientName = pointOfContactObject.fullName;
        this.recipients.recipientPersonId = pointOfContactObject.personId;
        this.recipients.recipientType = send;
        return this.recipients;
    }

    private setDuplicationValidation(msg: string) {
        this.validationMap.clear();
        switch (this.send) {
            case 'TO':
                this.validationMap.set('duplicateRecipientTO', msg);
                break;
            case 'CC':
                this.validationMap.set('duplicateRecipientCC', msg);
                break;
            case 'BCC':
                this.validationMap.set('duplicateRecipientBCC', msg);
                break;
            default:
        }
    }


    setElasticPerson() {
        this.elasticSearchOptionsForTo = this._elasticConfig.getElasticForPerson();
        // this.elasticSearchOptionsForCc = this._elasticConfig.getElasticForPerson();
        // this.elasticSearchOptionsForBcc = this._elasticConfig.getElasticForPerson();
    }

    clearNotifyFields() {
        this.notificationObject.message = '';
        this.notificationObject.subject = '';
        this.notificationObject.recipients = [];
        this.clearField = new String('true');
        this.validationMap.clear();
        this.defaultTemplateLabel = null;
    }

    removeRecipient(data) {
        const INDEX = this.notificationObject.recipients.findIndex(item => ((data.recipientPersonId && item.recipientPersonId === data.recipientPersonId)));
        this.notificationObject.recipients.splice(INDEX, 1);
    }

    redirectToProjectDetails(): void {
        this.commonService.redirectToProjectDetails(this.dataForNotificationSlider?.projectDetails?.projectId, this.dataForNotificationSlider?.projectDetails?.projectTypeCode);
    }
}
