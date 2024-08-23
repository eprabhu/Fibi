import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { CoiService } from '../services/coi.service';
import { Subscription } from 'rxjs';
import { DefineRelationshipService } from './services/define-relationship.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-define-relationship',
    templateUrl: './define-relationship.component.html',
    styleUrls: ['./define-relationship.component.scss'],
})
export class DefineRelationshipComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];

    constructor(private _coiService: CoiService,
                public defineRelationService: DefineRelationshipService) { }

    ngOnInit() {
        setTimeout(() => {
            this.defineRelationService.configureScrollSpy();
        }, 200);
        this.listenDisclosureHeaderChange();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private listenDisclosureHeaderChange(): void {
        this.$subscriptions.push(
            this._coiService.headerHeightChange$.subscribe((event: any) => {
                if (event) {
                    this.defineRelationService.setHeight();
                }
            })
        );
    }

}
