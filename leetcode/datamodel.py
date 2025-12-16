class List:
    def __init__(self, items):
        self.items = items

    def __getitem__(self, index):
        return self.items[index]

    def __setitem__(self, index, value):
        self.items[index] = value

    def __len__(self):
        return len(self.items)

    def append(self, value):
        self.items.append(value)

    def __repr__(self):
        return repr(self.items)
