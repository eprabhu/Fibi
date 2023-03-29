import {CommonModule} from '@angular/common';
import {RelationshipComponent} from './relationship.component';
import {RouterModule, Routes} from "@angular/router";
import {DefineRelationComponent} from './define-relation/define-relation.component';
import {FormsModule} from '@angular/forms';

import {NgModule} from '@angular/core';
import {EntityRelationshipService} from "./entity-relationship/entity-relationship.service";


export const relationshipRoutes: Routes = [
    {
        path: '', component: RelationshipComponent,
        children: [
            {path: '', redirectTo: 'entity-relation', pathMatch: 'full'},
            {
                path: 'entity-relation',
                loadChildren: () => import('./entity-relationship/entity-relationship.module')
                    .then(m => m.EntityRelationshipModule)
            }
        ]
    }
];

@NgModule({

    declarations: [
        RelationshipComponent,
        DefineRelationComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(relationshipRoutes),
        FormsModule
    ],
    providers: [EntityRelationshipService]
})
export class RelationshipModule {
}
