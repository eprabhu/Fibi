import { Component, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { AttachmentsService } from './attachments.service';
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { environment } from '../../../environments/environment';

@Component({
    selector: 'app-attachments',
    templateUrl: './attachments.component.html',
    styleUrls: ['./attachments.component.scss']
})
export class AttachmentsComponent implements OnInit {

    attachmentLists = [];
    isFirstTimeLoad = false;
    $subscriptions: Subscription[] = [];
    deployMap = environment.deployUrl;

    constructor(private _attachmentService: AttachmentsService, private _commonService: CommonService) { }

    ngOnInit() {
        this.getAllAttachments();
        this.updateNewAttachment();
    }

    updateNewAttachment() {
        this.$subscriptions.push(this._commonService.$updateLatestAttachment.subscribe((newlyAddedAttachments: any) => {
            if (newlyAddedAttachments && newlyAddedAttachments.length) {
                newlyAddedAttachments.forEach((ele: any) => {
                    this.attachmentLists.unshift(ele);
                })
            }
        }))
    }

    getAllAttachments() {
        this.isFirstTimeLoad = true;
        this.$subscriptions.push(this._attachmentService.fetchAllAttachmentsForPerson(this._commonService.getCurrentUserDetail('personId')).subscribe((data: any) => {
            if (data) {
                this.attachmentLists = data;
                this.isFirstTimeLoad = false;
            }
        }, err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, "Error in fetching attachment list");
        }));
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}