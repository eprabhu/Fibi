import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';

import { RelationshipComponent } from './relationship.component';
import { EntityRelationshipService } from './entity-relationship/entity-relationship.service';


export const relationshipRoutes = [
    { path: '', component: RelationshipComponent,
      children: [
        { path: '', redirectTo: 'entity-relation', pathMatch: 'full'},
        {path : 'entity-relation',
        loadChildren: () => import('./entity-relationship/entity-relationship.module')
        .then(m => m.EntityRelationshipModule)}
      ]
    }
  ];

@NgModule({
    imports: [
        CommonModule,
        RouterModule.forChild(relationshipRoutes)
    ],
    declarations: [RelationshipComponent],
    providers: [EntityRelationshipService]
})
export class RelationshipModule { }
