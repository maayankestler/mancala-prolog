#!/usr/bin/env python

# Programmer - Maayan Kestler
#  File Name - gui.py
# Description - mancala game with gui, use the prolog code (mancala.pl) for doing moves ad computer playing logic
# Synopsys - after installing all the pip packages in requirement.txt just run this code and start to play

from pyswip import Prolog
import pygame
import pygame.transform
import pygame_menu
import time

# pygame init
pygame.init()
pygame.display.set_caption('mancala')

# prolog init
prolog = Prolog()
prolog.consult("mancala.pl")


class MancalaGame:
    def __init__(self, player1=None, player2=None, pits_number=6, pieces_in_pit=4, screen_width=1200, screen_height=500, menu_height=50):
        """
        create mancala game instance
        @param player1: the first MancalaPlayer
        @param player2: the second MancalaPlayer
        @param pits_number: how much pits will be in each row
        @param pieces_in_pit: how much pieces will be in each pit
        @param screen_width: the width of the screen in pixels
        @param screen_height: the height of the screen in pixels
        @param menu_height: the height of the upper and lower menus
        """
        self.player1 = player1
        self.player2 = player2
        self.player1_score = 0
        self.player2_score = 0
        self.players = [player1, player2]
        self.current_player_number = 1  # 1 for player1 and 2 for player2
        self.pits_number = pits_number
        self.pieces_in_pit = pieces_in_pit
        self.board = [[pieces_in_pit] * pits_number, [pieces_in_pit] * pits_number]  # the board of the game
        # self.board = [[0, 1, 0, 1, 1, 0], [5, 1, 0, 0, 0, 1]]  # example
        self.screen_width = screen_width
        self.screen_height = screen_height
        self.screen = pygame.display.set_mode([self.screen_width, self.screen_height])  # the pygmae screen
        self.menu_width = screen_width
        self.menu_height = menu_height
        self.result_pit_width = int(screen_width / 8)  # the width of the pit that represent the score
        self.pits_surface_width = screen_width - 2 * self.result_pit_width  # the width in screen of the non result pits
        self.pits_surface_height = int(screen_height - 2 * menu_height)  # the height in screen of the non result pits
        self.pits_surface = pygame.Surface((self.pits_surface_width, self.pits_surface_height))
        self.pits_board = [row[:] for row in self.board]  # the pits board hold the drawing objects of the pits
        self.pit_surface_width = int(self.pits_surface_width / pits_number)  # the pixel width of each pit
        self.pit_surface_height = int(self.pits_surface_height / 2)  # the height width of each pit

    def start_menu(self):
        """
        display the menu before the game start
        """
        funcs_list = [('human', ''), ('alphabeta', 'alphabeta_ai'), ('random', 'random_ai')]
        alphabeta_levels_list = [('Very easy', 1), ('Easy', 3), ('Medium', 5), ('Hard', 7), ('Very hard', 9)]
        self.player1 = MancalaPlayer(name="Maayan", colour=(0, 0, 180), func="", extra_args=[1])
        self.player2 = MancalaPlayer(name="computer", colour=(0, 128, 0), func="alphabeta_ai", extra_args=[1])

        def start_the_game():
            if self.player1.func != "alphabeta_ai":
                self.player1.extra_args = []
            if self.player2.func != "alphabeta_ai":
                self.player2.extra_args = []
            menu.disable()

        menu = pygame_menu.Menu(self.screen_height, self.screen_width, 'Welcome', theme=pygame_menu.themes.THEME_BLUE)
        # player1 configs
        menu.add_text_input('player1 name: ', default=self.player1.name, onchange=self.player1.set_name)
        menu.add_selector('player1 func: ', funcs_list, default=0, onchange=self.player1.set_func)
        menu.add_selector('player1 level: ', alphabeta_levels_list, onchange=self.player1.set_alphabeta_level)
        menu.add_color_input('player1 color: ', color_type='rgb', default=self.player1.colour, onchange=self.player1.set_colour, font_size=18)
        # player2 configs
        menu.add_text_input('player2 name: ', default=self.player2.name, onchange=self.player2.set_name)
        menu.add_selector('player2 func: ', funcs_list, default=1, onchange=self.player2.set_func)
        menu.add_selector('player2 level: ', alphabeta_levels_list, default=0, onchange=self.player2.set_alphabeta_level)
        menu.add_color_input('player2 color: ', color_type='rgb', default=self.player2.colour, onchange=self.player2.set_colour, font_size=18)

        menu.add_button('Play', start_the_game)
        menu.add_button('Quit', pygame_menu.events.EXIT)

        menu.mainloop(self.screen)

    def end_menu(self, winner_text="", winner_colour=(0, 0, 0)):
        """
        the menu to display at the end of the game
        @param winner_text: the text descrive the winner of the last gmme
        @param winner_colour: the colour of the winner_text
        """
        def play_again():
            self.__init__(self.player1, self.player2)
            self.play(self.player1, self.player2)

        def new_game():
            self.__init__()
            self.play()

        menu = pygame_menu.Menu(int(self.screen_height/2), int(self.screen_width/2), 'Welcome', theme=pygame_menu.themes.THEME_BLUE)
        menu.add_label(winner_text, font_size=20, font_color=winner_colour)
        menu.add_button('Play again', play_again)
        menu.add_button('New game', new_game)
        menu.add_button('Quit', pygame_menu.events.EXIT)

        menu.mainloop(self.screen)

    def play(self, player1=None, player2=None):
        """
        play the mancala game
        @param player1: the first MancalaPlayer
        @param player2: the second MancalaPlayer
        """
        # if no players input use the start menu to define them
        if not player1 or not player2:
            self.start_menu()
        else:
            self.player1 = player1
            self.player2 = player2
        self.players = [self.player1, self.player2]

        up_menu = pygame.Surface((self.menu_width, self.menu_height))
        up_menu.fill((200, 200, 200))
        down_menu = pygame.Surface((self.menu_width, self.menu_height))
        down_menu.fill((200, 200, 200))
        players_name_font = pygame.font.SysFont("monospace", 30)

        self.update_pits_board()

        # Run until the game finished or the user asks to quit
        running = True
        while running:
            self.screen.fill((255, 255, 255))  # Fill the background with white
            current_player = self.players[self.current_player_number - 1]

            # catch game events
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False

                # handle mouse hover
                if event.type == pygame.MOUSEMOTION and not current_player.func:
                    pos = pygame.mouse.get_pos()
                    sprites = self.pits_board[self.current_player_number - 1]

                    # get a list of all sprites that are under the mouse cursor
                    hovered_sprites = [s for s in sprites if s.rect.collidepoint(pos)]
                    if len(hovered_sprites) > 0:
                        pygame.mouse.set_cursor(*pygame.cursors.broken_x)
                    else:
                        pygame.mouse.set_cursor(*pygame.cursors.arrow)

                # handle left click event
                if event.type == pygame.MOUSEBUTTONUP and event.button == 1 and not current_player.func:
                    pos = pygame.mouse.get_pos()
                    sprites = self.pits_board[self.current_player_number - 1]

                    # get a list of all sprites that are under the mouse cursor when click
                    clicked_sprites = [s for s in sprites if s.rect.collidepoint(pos)]
                    if len(clicked_sprites) > 0:
                        clicked_sprites[0].click()

            #  if this player is not a human
            if current_player.func:
                pygame.mouse.set_cursor(*pygame.cursors.arrow)
                start_time = time.time()

                # if current_player.extra_args:
                #     pit_index = self.prolog_ai_move(current_player.func, current_player.extra_args)
                # else:
                #     pit_index = self.prolog_ai_move(current_player.func)
                pit_index = self.prolog_ai_move(current_player.func, current_player.extra_args)
                self.do_move(pit_index)
                exec_time = time.time() - start_time
                # make delay
                if exec_time < 1:
                    time.sleep(1 - exec_time)

            self.screen.blit(self.pits_surface, (self.result_pit_width, self.menu_height))

            player1_result_pit = PitSurface(self.player1_score, self.result_pit_width, self.pit_surface_height * 2,
                                            colour=self.player1.colour).surface
            player2_result_pit = PitSurface(self.player2_score, self.result_pit_width, self.pit_surface_height * 2,
                                            colour=self.player2.colour).surface
            self.screen.blit(player1_result_pit, (0, self.menu_height))
            self.screen.blit(player2_result_pit, (self.screen_width - self.result_pit_width, self.menu_height))

            if self.current_player_number == 1:
                label = players_name_font.render(self.player1.name, 1, self.player1.colour)
                label_rect = label.get_rect(center=(int(self.menu_width / 2), int(self.menu_height / 2)))
                up_menu.blit(label, label_rect)
                down_menu.fill((200, 200, 200))
            elif self.current_player_number == 2:
                label = players_name_font.render(self.player2.name, 1, self.player2.colour)
                label_rect = label.get_rect(center=(int(self.menu_width / 2), int(self.menu_height / 2)))
                down_menu.blit(label, label_rect)
                up_menu.fill((200, 200, 200))
            self.screen.blit(up_menu, (0, 0))
            self.screen.blit(down_menu, (0, self.screen_height - self.menu_height))

            # Flip the display
            pygame.display.flip()

            winner = self.check_winner()
            if winner:
                try:
                    winner_text = f"The winner is {self.players[winner - 1].name}!!!!"
                    winner_colour = self.players[winner - 1].colour
                except IndexError:
                    winner_text = "The game end with tie"
                    winner_colour = (0, 0, 0)
                print(winner_text)
                running = False
                time.sleep(2)
                self.end_menu(winner_text, winner_colour)

        pygame.quit()

    def do_move(self, pit_index):
        """
        do one move in the game
        @param pit_index: the index of the pit the player want to play
        """
        prolog_query = list(prolog.query(f"do_move({self.board}, {self.current_player_number}, {self.player1_score}, {self.player2_score}, {pit_index}, NewBoard, NewPlayer1Score, NewPlayer2Score, NextPlayer)"))
        if len(prolog_query) > 0:
            result = prolog_query[0]
            self.board = result["NewBoard"]
            self.player1_score = result["NewPlayer1Score"]
            self.player2_score = result["NewPlayer2Score"]
            print(f"board: {self.board}, {self.player1.name} score: {self.player1_score}, {self.player2.name} score: {self.player2_score} turn: {self.current_player_number}")
            self.current_player_number = result["NextPlayer"]
            self.update_pits_board()
            if sum(self.board[self.current_player_number - 1]) == 0:
                current_player_number = (self.current_player_number % 2) + 1

    def update_pits_board(self):
        """
        update the pits_board (that hold the drawing objects of the pits) by board (only numbers)
        """
        for j in range(len(self.board[0])):
            pit_surface = PitSurface(self.board[0][j], self.pit_surface_width, self.pit_surface_height, j, self.player1.colour, do_move=self.do_move)
            pit_surface.rect.move_ip(self.result_pit_width + self.pit_surface_width * (len(self.board[0]) - j - 1), self.pit_surface_height * 0)
            self.pits_board[0][j] = pit_surface
            self.pits_surface.blit(self.pits_board[0][j].surface, (self.pit_surface_width * (len(self.board[0]) - j - 1), self.pit_surface_height * 0))

        for j in range(len(self.board[1])):
            pit_surface = PitSurface(self.board[1][j], self.pit_surface_width, self.pit_surface_height, j, self.player2.colour, do_move=self.do_move)
            pit_surface.rect.move_ip(self.result_pit_width + self.pit_surface_width * j, self.pit_surface_height * 1)
            self.pits_board[1][j] = pit_surface
            self.pits_surface.blit(self.pits_board[1][j].surface,(self.pit_surface_width * j, self.pit_surface_height * 1))

    def check_winner(self):
        """
        check if the game ended and who won
        @return: 0 if the game did not end wet,
                 1 if player1 won,
                 2 if player2 won,
                 3 if the game end with tie
        """
        if sum(self.board[0]) + sum(self.board[1]) == 0:
            if self.player1_score > self.player2_score:
                return 1
            elif self.player1_score < self.player2_score:
                return 2
            else:
                return 3
        else:
            return 0

    def prolog_ai_move(self, prolog_func, extra_args=[]):
        """
        do a move for the computer
        @param prolog_func: the func from mancala.pl to use
        @param extra_args: use the add more args to the prolog_func (for example alphabeta depth)
        @return: the index of the pit to play
        """
        if extra_args:
            query_string = f"{prolog_func}({self.board}, {self.current_player_number}, Pit, {str(extra_args)[1:-1]})"
        else:
            query_string = f"{prolog_func}({self.board}, {self.current_player_number}, Pit)"
        prolog_query = list(prolog.query(query_string))
        pit = prolog_query[0]["Pit"]
        if isinstance(pit, int):
            return pit
        else:
            raise ValueError(f"got {pit} as pit index from {prolog_func}, excepted an int")


# represent a mancala player
class MancalaPlayer:
    def __init__(self, name="Maayan", colour=(0, 0, 180), func="", extra_args=[]):
        self.name = name
        self.func = func
        self.colour = colour
        self.extra_args = extra_args

    def set_func(self, func_label, func_name):
        self.func = func_name

    def set_name(self, name):
        self.name = name

    def set_colour(self, colour):
        self.colour = colour

    def set_extra_args(self, extra_args):
        self.extra_args = extra_args

    def set_alphabeta_level(self, label, depth):
        self.set_extra_args([depth])


class PitSurface(pygame.sprite.Sprite):
    def __init__(self, pieces_amount, width, height, index=-1, colour=(0, 0, 0), do_move=None):
        super(PitSurface, self).__init__()
        self.pieces_amount = pieces_amount
        self.index = index + 1
        image = pygame.image.load("square-the-circle.jpg").convert()
        self.surface = pygame.transform.scale(image, (width, height))
        self.rect = self.surface.get_rect()
        self.font = pygame.font.SysFont("monospace", 30)
        self.label = self.font.render(str(self.pieces_amount), 1, colour)
        label_rect = self.label.get_rect(center=(int(width / 2), int(height / 2)))
        self.surface.blit(self.label, label_rect)
        self.do_move = do_move

    def click(self):
        """
        this function will be called when the pit is clicked
        """
        if self.index >= 0:
            self.do_move(self.index)


if __name__ == '__main__':
    try:
        import ctypes

        # calc game's screen resolution by the monitor resolution
        user32 = ctypes.windll.user32
        monitor_width = user32.GetSystemMetrics(0)
        monitor_height = user32.GetSystemMetrics(1)
        game = MancalaGame(screen_width=int(monitor_width*5/8), screen_height=int(monitor_height*5/11))
    except:
        game = MancalaGame()  # use default screen resolution

    game.play()
