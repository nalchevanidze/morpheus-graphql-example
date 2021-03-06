# Morpheus GraphQL Example

## Getting Started

- build app

  ```sh
  stack install 
  ```

- run Server after build succeed.

  ```sh
  - uhh-example
  ```

- visit: http://localhost:3000/

- query:

  ```gql
  {
    deity(id: "morpheus") {
      name
      power
    }
    characters {
      __typename
      ... on Deity {
        name
      }
      ... on Titan {
        name
      }
    }
  }
  ```

- response:

  ```json
  {
    "data": {
      "deity": {
        "name": "Morpheus",
        "power": [
          "Shapeshifting"
        ]
      },
      "characters": [
        {
          "__typename": "UnknownCreature"
        },
        {
          "__typename": "Deity",
          "name": "Morpheus"
        },
        {
          "__typename": "Titan",
          "name": "Prometheus"
        }
      ]
    }
  }
  ```