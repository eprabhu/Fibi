import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { DefineRelationshipService } from './services/define-relationship.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../../common/services/common.service';
import { GlobalEventNotifier } from '../../common/services/coi-common.interface';

@Component({
    selector: 'app-define-relationship',
    templateUrl: './define-relationship.component.html',
    styleUrls: ['./define-relationship.component.scss'],
})
export class DefineRelationshipComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    isExpandRightNav = true;

    constructor(private _commonService: CommonService,
                public defineRelationshipService: DefineRelationshipService) { }

    ngOnInit(): void {
        setTimeout(() => {
            this.defineRelationshipService.configureScrollSpy();
        }, 200);
        this.listenDisclosureHeaderChange();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private listenDisclosureHeaderChange(): void {
        this.$subscriptions.push(
            this._commonService.$globalEventNotifier.subscribe((event: GlobalEventNotifier) => {
                if (event.uniqueId === 'COI_DISCLOSURE_HEADER_RESIZE') {
                    this.defineRelationshipService.setHeight();
                    if (event.content.isResize) {
                        this.isExpandRightNav = true;
                    }
                }
            })
        );
    }

}
