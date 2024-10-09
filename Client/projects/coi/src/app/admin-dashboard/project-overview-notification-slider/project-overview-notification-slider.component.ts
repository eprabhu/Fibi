import { Component, EventEmitter, HostListener, Input, OnInit, Output } from '@angular/core';
import { closeCoiSlider, getFormattedSponsor, openCoiSlider } from '../../common/utilities/custom-utilities';
import { ProjectOverviewService } from '../project-overview.service';
import { CommonService } from '../../common/services/common.service';
import * as DecoupledEditor from '@ckeditor/ckeditor5-build-decoupled-document';
import { EDITOR_CONFIURATION, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from 'projects/fibi/src/app/app-constants';
import { NotificationTypeRO, NotificationObject, ProjectOverviewDetails, ContactPersonDetails, PROJECT_NOTIFICATION_SLIDER_ID, Recipient, RecipientGroup } from '../admin-dashboard.interface';
import { Subscription } from 'rxjs';
import { deepCloneObject, removeUnwantedTags } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { ElasticConfigService } from '../../common/services/elastic-config.service';
import { SharedProjectDetails } from '../../common/services/coi-common.interface';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';

@Component({
    selector: 'app-project-overview-notification-slider',
    templateUrl: './project-overview-notification-slider.component.html',
    styleUrls: ['./project-overview-notification-slider.component.scss']
})
export class ProjectOverviewNotificationSliderComponent implements OnInit {

    @Input() notificationSliderData: { projectDetailsForSlider: ProjectOverviewDetails, keyPersonIndex: number };
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
    notificationTypeRO: NotificationTypeRO = new NotificationTypeRO();
    notificationObject: NotificationObject = new NotificationObject();
    defaultKeypersonDetails = new Recipient(); 
    getFormattedSponsor = getFormattedSponsor;
    public Editor = DecoupledEditor;
    editorConfig = EDITOR_CONFIURATION;
    validationMap = new Map<string, string>();
    notificationTemplates = [];
    templateOptions = 'EMPTY#EMPTY#false#false';
    selectedlTemplateLookUpList = [];
    isCcViewable: boolean = false;
    isBccViewable: boolean = false;
    isEditorFocused = false;
    projectDetails = new SharedProjectDetails();
    elasticSearchOptionsForTo: any = {};
    elasticSearchOptionsForCc: any = {};
    elasticSearchOptionsForBcc: any = {};
    clearField: String;
    $subscriptions: Subscription[] = [];
    recipientGroup: RecipientGroup = 'TO';

    constructor(private _projectOverviewService: ProjectOverviewService, private _commonService: CommonService,
        private _elasticConfig: ElasticConfigService
    ) { }

    ngOnInit(): void {
        openCoiSlider(PROJECT_NOTIFICATION_SLIDER_ID);
        this.fetchAllNotificationTemplates();
        this.setElasticPersonOptions();
        this.setDefaultKeypersonDetails();
        this.setDefaultDetails();
        this.notificationObject.recipients.push(this.defaultKeypersonDetails);
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private setDefaultDetails(): void {
        const { projectTypeCode, projectId } = this.notificationSliderData?.projectDetailsForSlider?.projectDetails || {};
        const disclosureId = this.notificationSliderData?.projectDetailsForSlider?.keyPersonDetails[this.notificationSliderData?.keyPersonIndex]?.disclosureId;
        Object.assign(this.notificationObject, { projectTypeCode, projectId, disclosureId });
        this.setProjectCardDetails();
    }

    private setProjectCardDetails(): void {
        const PROJECT_DETAILS = this.notificationSliderData?.projectDetailsForSlider?.projectDetails;
        if (PROJECT_DETAILS) {
            const {
                projectNumber, sponsorCode, primeSponsorCode, sponsorName, leadUnitName: homeUnitName, leadUnitNumber: homeUnitNumber, primeSponsorName, projectStatus, piName,
                projectStartDate, projectEndDate, projectBadgeColour, projectIcon, projectType, projectTypeCode, title: projectTitle, documentNumber, accountNumber, projectId
            } = PROJECT_DETAILS;

            this.projectDetails = {
                projectNumber, sponsorCode, primeSponsorCode, sponsorName, homeUnitName, homeUnitNumber, primeSponsorName, projectStatus, piName, projectStartDate,
                projectEndDate, projectBadgeColour, projectIcon, projectType, projectTypeCode, projectTitle, documentNumber, accountNumber, projectId
            };
        }
    }

    private setDefaultKeypersonDetails(): void {
        const KEYPERSON = this.notificationSliderData?.projectDetailsForSlider?.keyPersonDetails[this.notificationSliderData.keyPersonIndex];
        this.defaultKeypersonDetails.recipientName = KEYPERSON.keyPersonName;
        this.defaultKeypersonDetails.recipientPersonId = KEYPERSON.keyPersonId;
    }

    closeSlider(): void {
        closeCoiSlider(PROJECT_NOTIFICATION_SLIDER_ID);
        setTimeout(() => {
            this.clearNotificationFields();
            this.closePage.emit();
        }, 500);
    }

    public onReady(editor): void {
        editor.ui.getEditableElement().parentElement.insertBefore(
            editor.ui.view.toolbar.element,
            editor.ui.getEditableElement()
        );
    }

    private validateMandatoryFields(): void {
        this.notificationObject.message = removeUnwantedTags(this.notificationObject.message);
        this.validationMap.clear();
        const TRIMMED_SUBJECT = this.notificationObject.subject ? this.notificationObject.subject.trim() : '';
        const TO_RECIPIENTS = this.notificationObject.recipients.filter(e => e.recipientType === 'TO')
        if (TO_RECIPIENTS.length === 0) {
            this.validationMap.set('recipients', 'Please add at least one recipient.');
        }
        if (!TRIMMED_SUBJECT) {
            this.validationMap.set('subject', 'Please provide a subject.');
        }
        if (!this.notificationObject.message) {
            this.validationMap.set('message', 'Please provide the message.');
        }
    }

    sendNotification(): void {
        this.validateMandatoryFields();
        if (!this.validationMap.size) {
            this.$subscriptions.push(
                this._projectOverviewService.sendNotification(this.notificationObject).subscribe(response => {
                    if (response.includes('successfully')) {
                        this.closeSlider();
                        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Notification sent successfully.');
                    } else {
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Notification failed.');
                    }
                }, (_err) => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Sending Notification failed. Please try again.');
                }));
        }
    }

    /**
    * This host listener is used to keep the background scroll fixed at the top at all times.
    */
    @HostListener('window:scroll')
    onScroll(): void {
        if (this.isEditorFocused) {
            window.scrollTo(0, 0);
        }
    }

    onEditorFocus(): void {
        this.isEditorFocused = true;
    }

    onEditorBlur(): void {
        this.isEditorFocused = false;
    }

    selectedRecipient(value: ContactPersonDetails): void {
        this.validationMap.clear();
        if (value.email_addr) {
            if (!this.notificationObject.recipients.find(recipient => recipient.recipientPersonId === value.prncpl_id && recipient.recipientType === this.recipientGroup)) {
                this.selectedPOC(value);
            }
            else {
                this.duplicateRecipientValidation('Person already added. Please choose a different person.');
                this.setElasticPersonOptions();
                this.clearField = new String('true');
            }
        }
    }

    /**
    * used to get selected object from elastic search
    * @param {} value
    */
    selectedPOC(value): void {
        if (value) {
            const POINT_OF_CONTACT_OBJECT = {
                fullName: value.full_name,
                personId: value.prncpl_id
            };
            this.setElasticPersonOptions();
            this.addPerson(POINT_OF_CONTACT_OBJECT);
        }
    }

    private fetchAllNotificationTemplates(): void {
        this.notificationTemplates = [];
        this.notificationObject = new NotificationObject();
        this.$subscriptions.push(this._commonService.fetchAllNotifications(this.notificationTypeRO)
            .subscribe((data: any) => {
                if (data != null) {
                    this.notificationTemplates = data?.notificationTypes;
                }
            },
                (_err) =>
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Notification templates fetch failed. Please try again.')
            ));
    }

    private addPerson(pointOfContactObject: any): void {
        this.clearField = new String('true');
        if (pointOfContactObject.personId) {
            const RECIPIENT = this.addPersonDetails(pointOfContactObject, this.recipientGroup);
            if (RECIPIENT) {
                this.notificationObject.recipients.push(deepCloneObject(RECIPIENT));
            }
            pointOfContactObject.fullName = null;
            pointOfContactObject.personId = null;
        }
    }

    private addPersonDetails(pointOfContactObject, recipientGroup): any {
        const NEW_RECIPIENT = {
            recipientName: pointOfContactObject.fullName,
            recipientPersonId: pointOfContactObject.personId,
            recipientType: recipientGroup
        }
        return NEW_RECIPIENT
    }

    private duplicateRecipientValidation(msg: string): void {
        this.validationMap.clear();
        switch (this.recipientGroup) {
            case 'TO':
                this.validationMap.set('duplicateRecipientTo', msg);
                break;
            case 'CC':
                this.validationMap.set('duplicateRecipientCc', msg);
                break;
            case 'BCC':
                this.validationMap.set('duplicateRecipientBcc', msg);
                break;
            default:
        }
    }

    private setElasticPersonOptions(): void {
        this.elasticSearchOptionsForTo = this._elasticConfig.getElasticForPerson();
        this.elasticSearchOptionsForCc = this._elasticConfig.getElasticForPerson();
        this.elasticSearchOptionsForBcc = this._elasticConfig.getElasticForPerson();
    }

    clearNotificationFields(): void {
        this.notificationObject.message = '';
        this.notificationObject.subject = '';
        this.notificationObject.recipients = [];
        this.clearField = new String('true');
        this.validationMap.clear();
        this.selectedlTemplateLookUpList = [];
    }

    removeRecipient(recipient: any): void {
        const INDEX = this.notificationObject.recipients.findIndex(item =>
            ((recipient.recipientPersonId && item.recipientPersonId === recipient.recipientPersonId) && this.checkForRecipientType(item.recipientType, recipient.recipientType))
        );
        this.notificationObject.recipients.splice(INDEX, 1);
    }

    private checkForRecipientType(deleteUserRecipientType, notificationObjectRecipientType): boolean {
        return deleteUserRecipientType === notificationObjectRecipientType;
    }

    onTemplateSelect(event): void {
        const NOTIFICATION_TYPE_ID = event?.[0]?.notificationTypeId;
        NOTIFICATION_TYPE_ID && this.getMailPreview(Number(NOTIFICATION_TYPE_ID));
    }

    private getMailPreview(notificationTypeId) {
        const MAIL_PREVIEW_RO = {
            notificationTypeId: notificationTypeId,
            moduleCode: this.notificationTypeRO.moduleCode,
            moduleItemKey: this.notificationObject.disclosureId,
        }
        this.$subscriptions.push(this._projectOverviewService.getMailPreviewById(MAIL_PREVIEW_RO).subscribe((res: any) => {
            this.notificationObject.message = res.body;
            this.notificationObject.subject = res.subject;
        },
            (_err) =>
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Notification details fetch failed. Please try again.')
        ));
    }
}
