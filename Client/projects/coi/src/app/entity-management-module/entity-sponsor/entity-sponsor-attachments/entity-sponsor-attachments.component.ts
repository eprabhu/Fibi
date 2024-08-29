import { Component, Input, OnInit } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';
import { EntireEntityDetails } from '../../shared/entity-interface';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityAttachmentModalService } from '../../shared/entity-attachment-modal/entity-attachment-modal.service';
import { HTTP_SUCCESS_STATUS } from '../../../app-constants';

@Component({
    selector: 'app-entity-sponsor-attachments',
    templateUrl: './entity-sponsor-attachments.component.html',
    styleUrls: ['./entity-sponsor-attachments.component.scss']
})
export class EntitySponsorAttachmentsComponent implements OnInit {

    @Input() sectionName: any;
    @Input() sectionId: any;
    entityAttachmentHelpText: string = '';
    isOpenSponsorAttachmentModal = false;
    entityDetails: any;
    entitySponsorAttachments: any;
    $subscriptions: Subscription[] = [];
    attachmentIndex: number;


    constructor(public commonService: CommonService,private _dataStoreService: EntityDataStoreService,
        private _entityAttachmentModalService: EntityAttachmentModalService,
    ) { }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    private getDataFromStore() {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        // if (this.entityDetails?.entityId != ENTITY_DATA?.entityDetails?.entityId) {
        //     this.fetchEntitySponsorDetails(ENTITY_DATA?.entityDetails?.entityId);
        // }
        this.entityDetails = ENTITY_DATA;
        this.entitySponsorAttachments = ENTITY_DATA?.entityAttachments;
        
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[] | 'ENTITY_RISK_TYPE') => {
                if (dependencies !==  'ENTITY_RISK_TYPE') {
                    this.getDataFromStore();
                }
            })
        );
    }

    closeModal(event: any): void {
        this.isOpenSponsorAttachmentModal = false;
        this.entityAttachmentHelpText = '';
    }

    openAttachmentModal() {
        this.isOpenSponsorAttachmentModal = true
        this.entityAttachmentHelpText = 'You can view and edit attachments under the sponsor tab.';
    }

    deleteAttachment(index: number){
            this.$subscriptions.push(this._entityAttachmentModalService.deleteAttachment().subscribe((data) => {
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Succesfully deleted');
            }))
    }

    downloadAttachments(attachment: any , index: number){
        const entityAttachmentId = attachment.entityAttachmentId;
        this.$subscriptions.push(this._entityAttachmentModalService.downloadAwardAttachment({entityAttachmentId}).subscribe(() => {
            this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Succesfully Downloaded');
            this.attachmentIndex = index;
        }))
    }
}
